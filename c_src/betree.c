#include <float.h>
#include <stdatomic.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "erl_nif.h"

// IMPORTANT! `NIF true` has to be defined before the use of any `be-tree`
// library headers. `NIF` controls memory allocation/de-allocation functions in
// `alloc.h`. The usage of different sets of allocation/de-allocation functions
// in the `be-tree` library and in the `erl-be-tree` will lead to memory leaks
// and/or crashes.
#define NIF true

#define USE_RESOURCE_FOR_REPORT_REASON

#include "dyn_arr.h"
#include "betree.h"
#include "debug.h"

// return values
static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_continue;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_bad_expr;
static ERL_NIF_TERM atom_bad_event;

static ERL_NIF_TERM atom_bad_arity;
static ERL_NIF_TERM atom_bad_id;
static ERL_NIF_TERM atom_bad_constant_list;
static ERL_NIF_TERM atom_bad_constant;
static ERL_NIF_TERM atom_bad_binary;
static ERL_NIF_TERM atom_failed;
static ERL_NIF_TERM atom_mem_alloc_failed;

// domain types
static ERL_NIF_TERM atom_int;
static ERL_NIF_TERM atom_int_list;
static ERL_NIF_TERM atom_int_enum;
static ERL_NIF_TERM atom_bin;
static ERL_NIF_TERM atom_bin_list;
static ERL_NIF_TERM atom_bool;
static ERL_NIF_TERM atom_float;
static ERL_NIF_TERM atom_frequency_caps;
static ERL_NIF_TERM atom_segments;
static ERL_NIF_TERM atom_int64;

// domain properties
static ERL_NIF_TERM atom_allow_undefined;
static ERL_NIF_TERM atom_disallow_undefined;

// values
static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_false;
static ERL_NIF_TERM atom_undefined;
static ERL_NIF_TERM atom_unknown;

static ErlNifResourceType *MEM_BETREE;
static ErlNifResourceType *MEM_SUB;
static ErlNifResourceType *MEM_EVENT;
static ErlNifResourceType *MEM_SEARCH_STATS;

struct sub {
  const struct betree_sub *sub;
};

struct evt {
  struct betree_event *event;
};

struct sub_info {
  ERL_NIF_TERM sub_id;
  size_t group_idx;  // direct index into betree_resource.group_ids (SIZE_MAX if no group)
  const struct betree_sub *sub_ptr;  // direct pointer to the betree_sub in the tree
};

typedef _Atomic(uint64_t) counter_t;

// betree and associate data
struct betree_resource {
  struct betree* betree;
  // subscription mapping and statistics
  struct sub_info* sub_index;
  size_t sub_capacity;
  size_t sub_count;
  counter_t* sub_stats;
  // group mapping and statistics
  ERL_NIF_TERM* group_ids; // array of unique group IDs
  size_t group_capacity;   // allocated capacity for groups
  size_t group_count;      // number of unique groups
  counter_t* group_stats;  // group-level stats: [group][attr_domain]
};

// stats accumulator for resource-based statistics
struct stats_resource {
  struct betree_resource* betree_res; // reference to betree resource
  betree_var_t* group_results;        // per-group results: 0=no value, 1=passed, (var_idx+2)=failure reason
  size_t attr_domain_count;           // number of attribute domains
  bool active;                        // whether accumulator is active
};

// thread-local dynamic arrays for subs and rets
static __thread ERL_NIF_TERM* subs = NULL;
static __thread ERL_NIF_TERM* rets = NULL;
static __thread size_t allocated_size = 0;
static __thread size_t subs_count = 0;

static ERL_NIF_TERM ids_from_report(ErlNifEnv *env,
                                    const struct report *report);

#include "alloc.h"
#include "hashmap.h"
#include "tree.h"
#include <assert.h>

static ERL_NIF_TERM make_atom(ErlNifEnv *env, const char *name) {
  ERL_NIF_TERM ret;

  if (enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1)) {
    return ret;
  }

  return enif_make_atom(env, name);
}

static void cleanup_betree(ErlNifEnv *env, void *obj) {
  (void)env;
  struct betree_resource *betree_res = obj;
  if (betree_res->betree->subs_data != NULL) {
    if (betree_res->betree->subs_data->array != NULL) {
      enif_free(betree_res->betree->subs_data->array);
    }
    enif_free(betree_res->betree->subs_data);
  }
  if (betree_res->sub_index != NULL) enif_free(betree_res->sub_index);
  if (betree_res->sub_stats != NULL) enif_free(betree_res->sub_stats);
  if (betree_res->group_ids != NULL) enif_free(betree_res->group_ids);
  if (betree_res->group_stats != NULL) enif_free(betree_res->group_stats);
  betree_deinit(betree_res->betree);
  enif_free(betree_res->betree);
}

static void cleanup_event(ErlNifEnv *env, void *obj) {
  (void)env;
  struct evt *evt = obj;
  betree_free_event(evt->event);
  evt->event = NULL;
}

static void cleanup_stats_accumulator(ErlNifEnv *env, void *obj) {
  (void)env;
  struct stats_resource *acc = obj;
  if (acc->group_results != NULL) {
    enif_free(acc->group_results);
  }
  if (acc->betree_res != NULL) {
    enif_release_resource(acc->betree_res);  // Release dependency on betree_res
  }
}

static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
  (void)priv_data;
  (void)load_info;

  atom_ok = make_atom(env, "ok");
  atom_continue = make_atom(env, "continue");
  atom_error = make_atom(env, "error");
  atom_bad_expr = make_atom(env, "bad_expr");
  atom_bad_event = make_atom(env, "bad_event");
  atom_bad_arity = make_atom(env, "bad_arity");
  atom_bad_id = make_atom(env, "bad_id");
  atom_bad_constant_list = make_atom(env, "bad_constant_list");
  atom_bad_constant = make_atom(env, "bad_constant");
  atom_bad_binary = make_atom(env, "bad_binary");
  atom_failed = make_atom(env, "failed");
  atom_mem_alloc_failed = make_atom(env, "mem_alloc_failed");

  atom_int = make_atom(env, "int");
  ;
  atom_int_list = make_atom(env, "int_list");
  atom_int_enum = make_atom(env, "int_enum");
  atom_bin = make_atom(env, "bin");
  atom_bin_list = make_atom(env, "bin_list");
  atom_bool = make_atom(env, "bool");
  ;
  atom_float = make_atom(env, "float");
  atom_frequency_caps = make_atom(env, "frequency_caps");
  atom_segments = make_atom(env, "segments");
  atom_int64 = make_atom(env, "int64");

  atom_allow_undefined = make_atom(env, "allow_undefined");
  atom_disallow_undefined = make_atom(env, "disallow_undefined");

  atom_true = make_atom(env, "true");
  atom_false = make_atom(env, "false");
  atom_undefined = make_atom(env, "undefined");
  atom_unknown = make_atom(env, "unknown");

  int flags =
      (int)((unsigned)ERL_NIF_RT_CREATE | (unsigned)ERL_NIF_RT_TAKEOVER);
  MEM_BETREE =
      enif_open_resource_type(env, NULL, "betree", cleanup_betree, flags, NULL);
  if (MEM_BETREE == NULL) {
    return -1;
  }
  // We don't own the betree_sub, betree will deinit it. No dtor
  MEM_SUB = enif_open_resource_type(env, NULL, "sub", NULL, flags, NULL);
  if (MEM_SUB == NULL) {
    return -1;
  }
  MEM_EVENT =
      enif_open_resource_type(env, NULL, "event", cleanup_event, flags, NULL);
  if (MEM_EVENT == NULL) {
    return -1;
  }
  MEM_SEARCH_STATS = enif_open_resource_type(
      env, NULL, "stats_resource", cleanup_stats_accumulator, flags, NULL);
  if (MEM_SEARCH_STATS == NULL) {
    return -1;
  }

  return 0;
}

static struct betree_resource *get_betree_resource(ErlNifEnv *env, const ERL_NIF_TERM term) {
  struct betree_resource *betree_res = NULL;
  enif_get_resource(env, term, MEM_BETREE, (void *)&betree_res);
  return betree_res;
}

static struct betree *get_betree(ErlNifEnv *env, const ERL_NIF_TERM term) {
  struct betree_resource *betree_res = get_betree_resource(env, term);
  return betree_res ? betree_res->betree : NULL;
}

static struct evt *get_evt(ErlNifEnv *env, const ERL_NIF_TERM term) {
  struct evt *evt = NULL;
  enif_get_resource(env, term, MEM_EVENT, (void *)&evt);
  return evt;
}

static struct sub *get_sub(ErlNifEnv *env, const ERL_NIF_TERM term) {
  struct sub *sub = NULL;
  enif_get_resource(env, term, MEM_SUB, (void *)&sub);
  return sub;
}

static struct stats_resource *get_stats_accumulator(ErlNifEnv *env, const ERL_NIF_TERM term) {
  struct stats_resource *acc = NULL;
  enif_get_resource(env, term, MEM_SEARCH_STATS, (void *)&acc);
  return acc;
}

// Helper function to find or add a group ID in the betree resource global mapping
static bool find_or_add_global_group(struct betree_resource *betree_res, ERL_NIF_TERM group_id) {
  // No group_id given
  if (enif_is_identical(group_id, atom_undefined)) {
    betree_res->sub_index[betree_res->sub_count].group_idx = SIZE_MAX; // No group
    return true;
  }

  // Look for existing group in global mapping
  for (size_t idx = 0; idx < betree_res->group_count; idx++) {
    if (enif_is_identical(betree_res->group_ids[idx], group_id)) {
      betree_res->sub_index[betree_res->sub_count].group_idx = idx;
      return true;
    }
  }

  // Need to add new group - check capacity
  if (betree_res->group_count >= betree_res->group_capacity) {
    size_t new_capacity = (betree_res->group_capacity == 0) ? 64 : betree_res->group_capacity * 2;
    ERL_NIF_TERM *new_group_ids = enif_realloc(betree_res->group_ids, new_capacity * sizeof(ERL_NIF_TERM));

    if (new_group_ids == NULL) {
      // Memory allocation failed, can't add new group
      betree_res->sub_index[betree_res->sub_count].group_idx = SIZE_MAX;
      return false;
    }

    betree_res->group_ids = new_group_ids;
    betree_res->group_capacity = new_capacity;
  }

  // Add new group
  betree_res->group_ids[betree_res->group_count] = group_id;
  betree_res->sub_index[betree_res->sub_count].group_idx = betree_res->group_count++;
  return true;
}

static char *alloc_string(ErlNifBinary bin) {
  size_t key_len = bin.size;
  char *key = enif_alloc((key_len + 1) * sizeof(*key));
  if (!key) {
    return NULL;
  }
  memcpy(key, bin.data, key_len);
  key[key_len] = 0;
  return key;
}

static bool get_binary(ErlNifEnv *env, ERL_NIF_TERM term, const char *name,
                       struct betree_variable **variable) {
  ErlNifBinary bin;
  if (!enif_inspect_binary(env, term, &bin)) {
    return false;
  }
  char *value = alloc_string(bin);
  *variable = betree_make_string_variable(name, value);
  enif_free(value);
  return true;
}

static bool get_boolean(ErlNifEnv *env, ERL_NIF_TERM term, const char *name,
                        struct betree_variable **variable) {
  (void)env;
  bool value = false;
  if (enif_is_identical(atom_true, term)) {
    value = true;
  } else if (enif_is_identical(atom_false, term)) {
    value = false;
  } else {
    return false;
  }

  *variable = betree_make_boolean_variable(name, value);

  return true;
}

static bool get_int(ErlNifEnv *env, ERL_NIF_TERM term, const char *name,
                    struct betree_variable **variable) {
  int64_t value;
  if (!enif_get_int64(env, term, &value)) {
    return false;
  }

  *variable = betree_make_integer_variable(name, value);

  return true;
}

static bool get_float(ErlNifEnv *env, ERL_NIF_TERM term, const char *name,
                      struct betree_variable **variable) {
  double value;
  if (!enif_get_double(env, term, &value)) {
    return false;
  }

  *variable = betree_make_float_variable(name, value);

  return true;
}

static bool get_bin_list(ErlNifEnv *env, ERL_NIF_TERM term, const char *name,
                         struct betree_variable **variable) {
  ERL_NIF_TERM head;
  ERL_NIF_TERM tail = term;
  ErlNifBinary bin;
  const char *value;
  unsigned int length;

  if (!enif_get_list_length(env, term, &length)) {
    return false;
  }

  struct betree_string_list *list = betree_make_string_list(length);

  for (unsigned int i = 0; i < length; i++) {
    if (!enif_get_list_cell(env, tail, &head, &tail)) {
      betree_free_string_list(list);
      return false;
    }

    if (!enif_inspect_binary(env, head, &bin)) {
      betree_free_string_list(list);
      return false;
    }

    value = alloc_string(bin);
    betree_add_string(list, i, value);
    enif_free((char *)value);
  }

  *variable = betree_make_string_list_variable(name, list);

  return true;
}

static bool get_int_list(ErlNifEnv *env, ERL_NIF_TERM term, const char *name,
                         struct betree_variable **variable) {
  ERL_NIF_TERM head;
  ERL_NIF_TERM tail = term;
  int64_t value;
  unsigned int length;

  if (!enif_get_list_length(env, term, &length)) {
    return false;
  }

  struct betree_integer_list *list = betree_make_integer_list(length);

  for (unsigned int i = 0; i < length; i++) {
    if (!enif_get_list_cell(env, tail, &head, &tail)) {
      betree_free_integer_list(list);
      return false;
    }
    if (!enif_get_int64(env, head, &value)) {
      betree_free_integer_list(list);
      return false;
    }

    betree_add_integer(list, i, value);
  }

  *variable = betree_make_integer_list_variable(name, list);

  return true;
}

static bool get_frequency_cap(ErlNifEnv *env, ERL_NIF_TERM term,
                              struct betree_frequency_cap **ptr) {
  int cap_arity;
  int key_arity;
  ErlNifBinary bin;
  char *type_str = NULL;
  char *ns_str = NULL;
  bool success = true;

  const ERL_NIF_TERM *cap_content;
  const ERL_NIF_TERM *key_content;

  if (!enif_get_tuple(env, term, &cap_arity, &cap_content) || cap_arity != 3) {
    return false;
  }

  if (!enif_get_tuple(env, cap_content[0], &key_arity, &key_content) ||
      key_arity != 3) {
    return false;
  }

  if (!enif_inspect_binary(env, key_content[0], &bin)) {
    return false;
  }

  type_str = alloc_string(bin);

  uint32_t id;
  if (!enif_get_uint(env, key_content[1], &id)) {
    success = false;
    goto cleanup;
  }

  if (!enif_inspect_binary(env, key_content[2], &bin)) {
    success = false;
    goto cleanup;
  }

  ns_str = alloc_string(bin);

  uint32_t value;
  if (!enif_get_uint(env, cap_content[1], &value)) {
    success = false;
    goto cleanup;
  }

  bool timestamp_defined;
  int64_t timestamp = 0;
  if (enif_is_identical(atom_undefined, cap_content[2])) {
    timestamp_defined = false;
  } else if (enif_get_int64(env, cap_content[2], &timestamp)) {
    timestamp_defined = true;
  } else {
    success = false;
    goto cleanup;
  }
  struct betree_frequency_cap *frequency_cap = betree_make_frequency_cap(
      type_str, id, ns_str, timestamp_defined, timestamp, value);
  if (frequency_cap == NULL) {
    success = false;
    goto cleanup;
  }
  *ptr = frequency_cap;
cleanup:
  if (type_str != NULL) {
    enif_free(type_str);
  }
  if (ns_str != NULL) {
    enif_free(ns_str);
  }

  return success;
}

static bool get_frequency_caps_list(ErlNifEnv *env, ERL_NIF_TERM term,
                                    const char *name,
                                    struct betree_variable **variable) {
  ERL_NIF_TERM head;
  ERL_NIF_TERM tail = term;
  unsigned int length;

  if (!enif_get_list_length(env, term, &length)) {
    return false;
  }

  struct betree_frequency_caps *list = betree_make_frequency_caps(length);

  for (unsigned int i = 0; i < length; i++) {
    if (!enif_get_list_cell(env, tail, &head, &tail)) {
      betree_free_frequency_caps(list);
      return false;
    }
    struct betree_frequency_cap *frequency_cap;
    if (!get_frequency_cap(env, head, &frequency_cap)) {
      betree_free_frequency_caps(list);
      return false;
    }
    betree_add_frequency_cap(list, i, frequency_cap);
  }

  *variable = betree_make_frequency_caps_variable(name, list);

  return true;
}

static bool get_segment(ErlNifEnv *env, ERL_NIF_TERM term,
                        struct betree_segment **ptr) {
  int segment_arity;
  const ERL_NIF_TERM *segment_content;

  if (!enif_get_tuple(env, term, &segment_arity, &segment_content) ||
      segment_arity != 2) {
    return false;
  }

  int64_t id;
  if (!enif_get_int64(env, segment_content[0], &id)) {
    return false;
  }

  int64_t timestamp;
  if (!enif_get_int64(env, segment_content[1], &timestamp)) {
    return false;
  }

  *ptr = betree_make_segment(id, timestamp);

  return true;
}

static bool get_segments_list(ErlNifEnv *env, ERL_NIF_TERM term,
                              const char *name,
                              struct betree_variable **variable) {
  ERL_NIF_TERM head;
  ERL_NIF_TERM tail = term;
  unsigned int length;

  if (!enif_get_list_length(env, term, &length)) {
    return false;
  }

  struct betree_segments *list = betree_make_segments(length);

  for (unsigned int i = 0; i < length; i++) {
    if (!enif_get_list_cell(env, tail, &head, &tail)) {
      betree_free_segments(list);
      return false;
    }
    struct betree_segment *segment;
    if (!get_segment(env, head, &segment)) {
      betree_free_segments(list);
      return false;
    }

    betree_add_segment(list, i, segment);
  }

  *variable = betree_make_segments_variable(name, list);

  return true;
}

#define DOMAIN_NAME_LEN 256

static int get_rank(ErlNifEnv *env, ERL_NIF_TERM name, const ERL_NIF_TERM *ranks) {
  if (ranks == NULL)
    return 0;
  ERL_NIF_TERM ret;
  if (!enif_get_map_value(env, *ranks, name, &ret))
    return 0;
  int rank;
  if (!enif_get_int(env, ret, &rank))
    return 0;
  return rank;
}

static bool add_domains(ErlNifEnv *env, struct betree *betree,
                        ERL_NIF_TERM list, unsigned int list_len, const ERL_NIF_TERM *ranks) {
  ERL_NIF_TERM head;
  ERL_NIF_TERM tail = list;
  const ERL_NIF_TERM *tuple;
  int tuple_len;
  char domain_name[DOMAIN_NAME_LEN];
  bool allow_undefined;
  for (unsigned int i = 0; i < list_len; i++) {
    if (!enif_get_list_cell(env, tail, &head, &tail)) {
      return false;
    }

    if (!enif_get_tuple(env, head, &tuple_len, &tuple)) {
      return false;
    }
    if (tuple_len < 3) {
      return false;
    }

    if (!enif_get_atom(env, tuple[0], domain_name, DOMAIN_NAME_LEN,
                       ERL_NIF_LATIN1)) {
      return false;
    }

    if (enif_is_identical(atom_allow_undefined, tuple[2])) {
      allow_undefined = true;
    } else if (enif_is_identical(atom_disallow_undefined, tuple[2])) {
      allow_undefined = false;
    } else {
      return false;
    }

    if (enif_is_identical(atom_int, tuple[1]) ||
        enif_is_identical(atom_int64, tuple[1])) {
      int64_t min = INT64_MIN;
      int64_t max = INT64_MAX;
      if (tuple_len == 5) {
        if (!enif_get_int64(env, tuple[3], &min) ||
            !enif_get_int64(env, tuple[4], &max)) {
          return false;
        }
      }
      betree_add_ranked_integer_variable(
        betree, domain_name, allow_undefined, min, max, get_rank(env, tuple[0], ranks));
    } else if (enif_is_identical(atom_int_list, tuple[1])) {
      int64_t min = INT64_MIN;
      int64_t max = INT64_MAX;
      if (tuple_len == 5) {
        int64_t min, max;
        if (!enif_get_int64(env, tuple[3], &min) ||
            !enif_get_int64(env, tuple[4], &max)) {
          return false;
        }
      }
      betree_add_ranked_integer_list_variable(
        betree, domain_name, allow_undefined, min, max, get_rank(env, tuple[0], ranks));
    } else if (enif_is_identical(atom_int_enum, tuple[1])) {
      size_t max = SIZE_MAX;
      if (tuple_len == 4) {
        uint64_t u64_max;
        if (!enif_get_uint64(env, tuple[3], &u64_max)) {
          return false;
        }
        max = (size_t)u64_max;
      }
      betree_add_ranked_integer_enum_variable(
        betree, domain_name, allow_undefined, max, get_rank(env, tuple[0], ranks));
    } else if (enif_is_identical(atom_bin, tuple[1])) {
      size_t max = SIZE_MAX;
      if (tuple_len == 4) {
        uint64_t u64_max;
        if (!enif_get_uint64(env, tuple[3], &u64_max)) {
          return false;
        }
        max = (size_t)u64_max;
      }
      betree_add_ranked_string_variable(
        betree, domain_name, allow_undefined, max, get_rank(env, tuple[0], ranks));
    } else if (enif_is_identical(atom_bin_list, tuple[1])) {
      size_t max = SIZE_MAX;
      if (tuple_len == 4) {
        uint64_t u64_max;
        if (!enif_get_uint64(env, tuple[3], &u64_max)) {
          return false;
        }
        max = (size_t)u64_max;
      }
      betree_add_ranked_string_list_variable(
        betree, domain_name, allow_undefined, max, get_rank(env, tuple[0], ranks));
    } else if (enif_is_identical(atom_bool, tuple[1])) {
      betree_add_ranked_boolean_variable(
        betree, domain_name, allow_undefined, get_rank(env, tuple[0], ranks));
    } else if (enif_is_identical(atom_float, tuple[1])) {
      double min = -DBL_MAX;
      double max = DBL_MAX;
      if (tuple_len == 5) {
        if (!enif_get_double(env, tuple[3], &min) ||
            !enif_get_double(env, tuple[4], &max)) {
          return false;
        }
      }
      betree_add_ranked_float_variable(
        betree, domain_name, allow_undefined, min, max, get_rank(env, tuple[0], ranks));
    } else if (enif_is_identical(atom_frequency_caps, tuple[1])) {
      betree_add_ranked_frequency_caps_variable(
        betree, domain_name, allow_undefined, get_rank(env, tuple[0], ranks));
    } else if (enif_is_identical(atom_segments, tuple[1])) {
      betree_add_ranked_segments_variable(
        betree, domain_name, allow_undefined, get_rank(env, tuple[0], ranks));
    } else {
      return false;
    }
  }

  // set attr_var.data to the atom representation of the attr
  for (size_t i=0; i<betree->config->attr_domain_count; i++) {
    struct attr_domain* ad = betree->config->attr_domains[i];
    ad->attr_var.data = (void*)enif_make_atom(env, ad->attr_var.attr);
  }

  return true;
}

static bool add_variables(ErlNifEnv *env, struct betree *betree,
                          struct betree_event *event, const ERL_NIF_TERM *tuple,
                          int tuple_len, size_t initial_domain_index) {
  // Start at 1 to not use the record name
  for (int i = 1; i < tuple_len; i++) {
    ERL_NIF_TERM element = tuple[i];
    if (enif_is_identical(atom_undefined, element)) {
      continue;
    }
    size_t domain_index = initial_domain_index + i - 1;
    struct betree_variable_definition def =
        betree_get_variable_definition(betree, domain_index);
    bool result;
    struct betree_variable *variable = NULL;
    switch (def.type) {
    case BETREE_BOOLEAN:
      result = get_boolean(env, element, def.name, &variable);
      break;
    case BETREE_INTEGER:
      result = get_int(env, element, def.name, &variable);
      break;
    case BETREE_FLOAT:
      result = get_float(env, element, def.name, &variable);
      break;
    case BETREE_STRING:
      result = get_binary(env, element, def.name, &variable);
      break;
    case BETREE_INTEGER_LIST:
      result = get_int_list(env, element, def.name, &variable);
      break;
    case BETREE_STRING_LIST:
      result = get_bin_list(env, element, def.name, &variable);
      break;
    case BETREE_SEGMENTS:
      result = get_segments_list(env, element, def.name, &variable);
      break;
    case BETREE_FREQUENCY_CAPS:
      result = get_frequency_caps_list(env, element, def.name, &variable);
      break;
    case BETREE_INTEGER_ENUM:
      result = get_int(env, element, def.name, &variable);
      break;
    default:
      result = false;
      break;
    }
    if (result == false) {
      if (variable != NULL) {
        betree_free_variable(variable);
      }
      return false;
    }
    betree_set_variable(event, domain_index, variable);
  }
  return true;
}

static ERL_NIF_TERM nif_betree_make(ErlNifEnv *env, int argc,
                                    const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM retval;
  if (argc < 1 || argc > 2) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  struct betree_resource *betree_res = enif_alloc_resource(MEM_BETREE, sizeof(*betree_res));
  if (betree_res == NULL) {
    retval = enif_make_tuple2(env, atom_error, atom_mem_alloc_failed);
    goto cleanup;
  }
  betree_res->betree = (struct betree*)enif_alloc(sizeof(struct betree));
  if (betree_res->betree == NULL) {
    retval = enif_make_tuple2(env, atom_error, atom_mem_alloc_failed);
    goto cleanup;
  }
  betree_init(betree_res->betree);
  betree_res->sub_index = NULL;
  betree_res->sub_capacity = 0;
  betree_res->sub_count = 0;
  betree_res->sub_stats = NULL;
  betree_res->group_ids = NULL;
  betree_res->group_capacity = 0;
  betree_res->group_count = 0;
  betree_res->group_stats = NULL;

  ERL_NIF_TERM term = enif_make_resource(env, betree_res);

  enif_release_resource(betree_res);

  unsigned int list_len;
  if (!enif_get_list_length(env, argv[0], &list_len)) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  const ERL_NIF_TERM *ranks = NULL;
  if (argc == 2) {
    if (!enif_is_map(env, argv[1])) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }
    ranks = &argv[1];
  }

  ERL_NIF_TERM head;
  ERL_NIF_TERM tail = argv[0];

  for (unsigned int i = 0; i < list_len; i++) {
    if (!enif_get_list_cell(env, tail, &head, &tail)) {
      return false;
    }

    unsigned int inner_list_len;
    if (!enif_get_list_length(env, head, &inner_list_len)) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }

    if (!add_domains(env, betree_res->betree, head, inner_list_len, ranks)) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }
  }

  retval = enif_make_tuple(env, 2, atom_ok, term);
cleanup:
  return retval;
}

// Make sure that clock_gettime will work correctly on different Linux flavors.
static int reverse_get_clock_type(int ct) {
  int res = CLOCK_MONOTONIC;
  switch (ct) {
  case 0:
    res = CLOCK_REALTIME;
    break;
  case 1:
    res = CLOCK_MONOTONIC;
    break;
  case 2:
    res = CLOCK_PROCESS_CPUTIME_ID;
    break;
  case 3:
    res = CLOCK_THREAD_CPUTIME_ID;
    break;
  }
  return res;
}

// Convert to microseconds
static ERL_NIF_TERM make_time(ErlNifEnv *env, const struct timespec *start,
                              const struct timespec *done) {
  ErlNifSInt64 tspent = (done->tv_sec - start->tv_sec) * 1000000 +
                        (done->tv_nsec - start->tv_nsec) / 1000;
  return enif_make_int64(env, tspent);
}

static ERL_NIF_TERM nif_betree_make_event(ErlNifEnv *env, int argc,
                                          const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM retval;
  int clock_type = 0;
  struct betree_event *event = NULL;
  struct evt *evt = NULL;

  if (argc != 3) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  if (!enif_get_int(env, argv[2], &clock_type)) {
    return enif_make_badarg(env);
  }
  clock_type = reverse_get_clock_type(clock_type);
  struct timespec start, done;
  clock_gettime(clock_type, &start);

  struct betree *betree = get_betree(env, argv[0]);
  if (betree == NULL) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  unsigned int list_len;
  if (!enif_get_list_length(env, argv[1], &list_len)) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  evt = enif_alloc_resource(MEM_EVENT, sizeof(*evt));
  if (evt == NULL) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }
  evt->event = event = betree_make_event(betree);
  ERL_NIF_TERM erl_event = enif_make_resource(env, evt);

  ERL_NIF_TERM head;
  ERL_NIF_TERM tail = argv[1];
  const ERL_NIF_TERM *tuple;

  size_t pred_index = 0;
  int tuple_len;

  for (unsigned int i = 0; i < list_len; i++) {
    if (!enif_get_list_cell(env, tail, &head, &tail)) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }

    if (!enif_get_tuple(env, head, &tuple_len, &tuple)) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }

    if (!add_variables(env, betree, event, tuple, tuple_len, pred_index)) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }
    pred_index += (tuple_len - 1);
  }

  retval = enif_make_tuple(env, 2, atom_ok, erl_event);

cleanup:
  if (evt != NULL) {
    enif_release_resource(evt);
  }
  clock_gettime(clock_type, &done);
  ERL_NIF_TERM etspent = make_time(env, &start, &done);
  return enif_make_tuple2(env, retval, etspent);
}

#define CONSTANT_NAME_LEN 256

static ERL_NIF_TERM nif_betree_make_sub(ErlNifEnv *env, int argc,
                                        const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM retval;
  ErlNifBinary bin;
  char *expr = NULL;
  size_t constant_count = 0;
  struct betree_constant **constants = NULL;
  if (argc != 4) {
    retval = enif_make_tuple2(env, atom_error, atom_bad_arity);
    goto cleanup;
  }

  struct betree *betree = get_betree(env, argv[0]);
  if (betree == NULL) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  betree_sub_t sub_id;
  if (!enif_get_uint64(env, argv[1], &sub_id)) {
    retval = enif_make_tuple2(env, atom_error, atom_bad_id);
    goto cleanup;
  }

  ERL_NIF_TERM head;
  ERL_NIF_TERM tail = argv[2];
  unsigned int length;

  if (!enif_get_list_length(env, argv[2], &length)) {
    retval = enif_make_tuple2(env, atom_error, atom_bad_constant_list);
    goto cleanup;
  }

  constants = enif_alloc(length * sizeof(*constants));
  if (constants == NULL) {
    retval = enif_make_tuple2(env, atom_error, atom_mem_alloc_failed);
    goto cleanup;
  }
  constant_count = length;
  for (unsigned int i = 0; i < length; i++) {
    constants[i] = NULL;
  }

  for (unsigned int i = 0; i < length; i++) {
    if (!enif_get_list_cell(env, tail, &head, &tail)) {
      retval = enif_make_tuple2(env, atom_error, atom_bad_constant_list);
      goto cleanup;
    }
    const ERL_NIF_TERM *tuple;
    int tuple_len;

    if (!enif_get_tuple(env, head, &tuple_len, &tuple)) {
      retval = enif_make_tuple4(env, atom_error, atom_bad_constant,
                                enif_make_int64(env, i), atom_unknown);
      goto cleanup;
    }

    if (tuple_len != 2) {
      retval = enif_make_tuple4(env, atom_error, atom_bad_constant,
                                enif_make_int64(env, i), atom_unknown);
      goto cleanup;
    }
    char constant_name[CONSTANT_NAME_LEN];
    if (!enif_get_atom(env, tuple[0], constant_name, CONSTANT_NAME_LEN,
                       ERL_NIF_LATIN1)) {
      retval = enif_make_tuple4(env, atom_error, atom_bad_constant,
                                enif_make_int64(env, i), atom_unknown);
      goto cleanup;
    }

    int64_t value;
    if (!enif_get_int64(env, tuple[1], &value)) {
      retval = enif_make_tuple4(env, atom_error, atom_bad_constant,
                                enif_make_int64(env, i),
                                make_atom(env, constant_name));
      goto cleanup;
    }
    constants[i] = betree_make_integer_constant(constant_name, value);
  }

  if (!enif_inspect_iolist_as_binary(env, argv[3], &bin)) {
    retval = enif_make_tuple2(env, atom_error, atom_bad_binary);
    goto cleanup;
  }
  expr = alloc_string(bin);
  if (expr == NULL) {
    retval = enif_make_tuple2(env, atom_error, atom_bad_binary);
    goto cleanup;
  }

  const struct betree_sub *betree_sub =
      betree_make_sub(betree, sub_id, constant_count,
                      (const struct betree_constant **)constants, expr);
  if (betree_sub == NULL) {
    retval = enif_make_tuple2(env, atom_error, atom_failed);
    goto cleanup;
  }

  struct sub *sub = enif_alloc_resource(MEM_SUB, sizeof(*sub));
  if (sub == NULL) {
    retval = enif_make_tuple2(env, atom_error, atom_mem_alloc_failed);
    goto cleanup;
  }
  sub->sub = betree_sub;

  ERL_NIF_TERM sub_term = enif_make_resource(env, sub);

  enif_release_resource(sub);

  retval = enif_make_tuple(env, 2, atom_ok, sub_term);
cleanup:
  if (expr != NULL) {
    enif_free(expr);
  }
  if (constants != NULL) {
    betree_free_constants(constant_count, constants);
    enif_free(constants);
  }

  return retval;
}

static bool prepare_subs_data(struct betree_resource* betree_res) {
  betree_res->betree->subs_data = (struct subs_data*)enif_alloc(sizeof(struct subs_data));
  if (betree_res->betree->subs_data == NULL) {
    goto cleanup;
  }

  betree_res->betree->subs_data->array = (void**)enif_alloc(betree_res->sub_count * sizeof(void*));
  if (betree_res->betree->subs_data->array == NULL) {
    goto cleanup;
  }

  // Allocate sub_stats
  size_t stats_array_size = betree_res->sub_count * betree_res->betree->config->attr_domain_count;
  betree_res->sub_stats = (counter_t*)enif_alloc(stats_array_size * sizeof(counter_t));
  if (betree_res->sub_stats == NULL) {
    goto cleanup;
  }
  // Initialize all elements to 0
  memset(betree_res->sub_stats, 0, stats_array_size * sizeof(counter_t));

  // Allocate group_stats if we have groups
  if (betree_res->group_count > 0) {
    size_t group_stats_size = betree_res->group_count * betree_res->betree->config->attr_domain_count;
    betree_res->group_stats = (counter_t*)enif_alloc(group_stats_size * sizeof(counter_t));
    if (betree_res->group_stats == NULL) {
      goto cleanup;
    }
    // Initialize group stats to 0
    memset(betree_res->group_stats, 0, group_stats_size * sizeof(counter_t));
  }

  betree_res->betree->subs_data->limit = betree_res->sub_count;
  betree_res->betree->subs_data->count = 0;
  betree_prepare_sub_data(betree_res->betree);
  return true;

cleanup:
  if (betree_res->group_stats != NULL) {
    enif_free(betree_res->group_stats);
    betree_res->group_stats = NULL;
  }
  if (betree_res->sub_stats != NULL) {
    enif_free(betree_res->sub_stats);
    betree_res->sub_stats = NULL;
  }
  if (betree_res->betree->subs_data != NULL) {
    if (betree_res->betree->subs_data->array != NULL) {
      enif_free(betree_res->betree->subs_data->array);
    }
    enif_free(betree_res->betree->subs_data);
    betree_res->betree->subs_data = NULL;
  }
  return false;
}

static bool index_sub(ErlNifEnv* env, struct betree_resource* betree_res, betree_sub_t sub_id, ERL_NIF_TERM group_id, const struct betree_sub* sub_ptr, void** ptr) {
  // Check if we need to allocate or grow the array
  if (betree_res->sub_count >= betree_res->sub_capacity) {
    size_t new_capacity = (betree_res->sub_capacity == 0) ? 64 : betree_res->sub_capacity * 2;
    struct sub_info* new_index;

    if (betree_res->sub_index == NULL) {
      // Initial allocation
      new_index = (struct sub_info*)enif_alloc(new_capacity * sizeof(*new_index));
    } else {
      // Reallocation
      new_index = (struct sub_info*)enif_realloc(betree_res->sub_index, new_capacity * sizeof(*new_index));
    }

    if (new_index == NULL) {
      // Memory allocation failed
      return false;
    }

    betree_res->sub_index = new_index;
    betree_res->sub_capacity = new_capacity;
  }

  // Add new element at the end of the array
  betree_res->sub_index[betree_res->sub_count].sub_id = enif_make_uint64(env, sub_id);
  betree_res->sub_index[betree_res->sub_count].sub_ptr = sub_ptr;

  // Find or add group and store the index
  if (!find_or_add_global_group(betree_res, group_id)) return false;

  // Return index of the newly added element
  *ptr = (void*)betree_res->sub_count;

  // Increment count
  betree_res->sub_count++;

  return true;
}

static ERL_NIF_TERM nif_betree_add_sub(ErlNifEnv *env, int argc,
                                        const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM retval;
  ErlNifBinary bin;
  char *expr = NULL;
  size_t constant_count = 0;
  struct betree_constant **constants = NULL;
  ERL_NIF_TERM group_id = atom_undefined;  // Default to undefined

  if (argc != 4 && argc != 5) {
    retval = enif_make_tuple2(env, atom_error, atom_bad_arity);
    goto cleanup;
  }

  // If 5 arguments provided, the 3rd argument is GroupId
  if (argc == 5) {
    group_id = argv[2];
  }

  struct betree_resource *betree_res = get_betree_resource(env, argv[0]);
  if (betree_res == NULL) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }
  struct betree *betree = betree_res->betree;

  betree_sub_t sub_id;
  if (!enif_get_uint64(env, argv[1], &sub_id)) {
    retval = enif_make_tuple2(env, atom_error, atom_bad_id);
    goto cleanup;
  }

  // Determine indices for constants and expression based on argc
  int constants_idx = (argc == 5) ? 3 : 2;
  int expr_idx = (argc == 5) ? 4 : 3;

  ERL_NIF_TERM head;
  ERL_NIF_TERM tail = argv[constants_idx];
  unsigned int length;

  if (!enif_get_list_length(env, argv[constants_idx], &length)) {
    retval = enif_make_tuple2(env, atom_error, atom_bad_constant_list);
    goto cleanup;
  }

  constants = enif_alloc(length * sizeof(*constants));
  if (constants == NULL) {
    retval = enif_make_tuple2(env, atom_error, atom_mem_alloc_failed);
    goto cleanup;
  }
  constant_count = length;
  for (unsigned int i = 0; i < length; i++) {
    constants[i] = NULL;
  }

  for (unsigned int i = 0; i < length; i++) {
    if (!enif_get_list_cell(env, tail, &head, &tail)) {
      retval = enif_make_tuple2(env, atom_error, atom_bad_constant_list);
      goto cleanup;
    }
    const ERL_NIF_TERM *tuple;
    int tuple_len;

    if (!enif_get_tuple(env, head, &tuple_len, &tuple)) {
      retval = enif_make_tuple4(env, atom_error, atom_bad_constant,
                                enif_make_int64(env, i), atom_unknown);
      goto cleanup;
    }

    if (tuple_len != 2) {
      retval = enif_make_tuple4(env, atom_error, atom_bad_constant,
                                enif_make_int64(env, i), atom_unknown);
      goto cleanup;
    }
    char constant_name[CONSTANT_NAME_LEN];
    if (!enif_get_atom(env, tuple[0], constant_name, CONSTANT_NAME_LEN,
                       ERL_NIF_LATIN1)) {
      retval = enif_make_tuple4(env, atom_error, atom_bad_constant,
                                enif_make_int64(env, i), atom_unknown);
      goto cleanup;
    }

    int64_t value;
    if (!enif_get_int64(env, tuple[1], &value)) {
      retval = enif_make_tuple4(env, atom_error, atom_bad_constant,
                                enif_make_int64(env, i),
                                make_atom(env, constant_name));
      goto cleanup;
    }
    constants[i] = betree_make_integer_constant(constant_name, value);
  }

  if (!enif_inspect_iolist_as_binary(env, argv[expr_idx], &bin)) {
    retval = enif_make_tuple2(env, atom_error, atom_bad_binary);
    goto cleanup;
  }
  expr = alloc_string(bin);
  if (expr == NULL) {
    retval = enif_make_tuple2(env, atom_error, atom_bad_binary);
    goto cleanup;
  }

  struct betree_sub *betree_sub =
      betree_make_sub(betree, sub_id, constant_count,
                      (const struct betree_constant **)constants, expr);
  if (betree_sub == NULL) {
    retval = enif_make_tuple2(env, atom_error, atom_failed);
    goto cleanup;
  }

  if (!index_sub(env, betree_res, sub_id, group_id, betree_sub, &betree_sub->data)) {
    retval = enif_make_tuple2(env, atom_error, atom_mem_alloc_failed);
    goto cleanup;
  }

  bool result = betree_insert_sub(betree, betree_sub);
  if (result) {
    retval = atom_ok;
  } else {
    retval = atom_error;
  }

cleanup:
  if (expr != NULL) {
    enif_free(expr);
  }
  if (constants != NULL) {
    betree_free_constants(constant_count, constants);
    enif_free(constants);
  }

  return retval;
}

static ERL_NIF_TERM nif_betree_insert_sub(ErlNifEnv *env, int argc,
                                          const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM retval;

  if (argc != 2) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  struct betree *betree = get_betree(env, argv[0]);
  if (betree == NULL) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  struct sub *sub = get_sub(env, argv[1]);
  if (sub == NULL) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  bool result = betree_insert_sub(betree, sub->sub);
  if (result) {
    retval = atom_ok;
  } else {
    retval = atom_error;
  }
cleanup:
  return retval;
}

static ERL_NIF_TERM nif_betree_search(ErlNifEnv *env, int argc,
                                      const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM retval;
  struct report *report = NULL;
  size_t pred_index = 0;
  struct betree_event *event = NULL;

  if (argc != 2) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  struct betree *betree = get_betree(env, argv[0]);
  if (betree == NULL) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  unsigned int list_len;
  if (!enif_get_list_length(env, argv[1], &list_len)) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  event = betree_make_event(betree);

  ERL_NIF_TERM head;
  ERL_NIF_TERM tail = argv[1];

  const ERL_NIF_TERM *tuple;
  int tuple_len;

  for (unsigned int i = 0; i < list_len; i++) {
    if (!enif_get_list_cell(env, tail, &head, &tail)) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }

    if (!enif_get_tuple(env, head, &tuple_len, &tuple)) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }

    if (!add_variables(env, betree, event, tuple, tuple_len, pred_index)) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }
    pred_index += (tuple_len - 1);
  }

  report = make_report();
  bool result = betree_search_with_event(betree, event, report);

  if (result == false) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  ERL_NIF_TERM res = ids_from_report(env, report);

  retval = enif_make_tuple2(env, atom_ok, res);
cleanup:
  if (event != NULL) {
    betree_free_event(event);
  }
  if (report != NULL) {
    free_report(report);
  }
  return retval;
}

static void acc_ret(void *arg, void *data, bool success, const void *context) {
  size_t index = (size_t)data;
  struct betree_resource* betree_res = (struct betree_resource*) arg;
  struct sub_info* info = &betree_res->sub_index[index];
  subs[index] = info->sub_id;
  const struct config* config = betree_res->betree->config;
  betree_var_t var_idx = (betree_var_t)context;
  const struct attr_domain *ad_ptr = var_idx < config->attr_domain_count ? config->attr_domains[var_idx] : NULL;
  const void *var = ad_ptr ? ad_ptr->attr_var.data : NULL;
  rets[index] = success ? atom_ok : var ? (ERL_NIF_TERM)var : atom_error;
}

static void acc_arr(void* arg, void** data, size_t count, const void* context) {
  struct betree_resource* betree_res = (struct betree_resource*) arg;
  const struct config* config = betree_res->betree->config;
  betree_var_t var_idx = (betree_var_t)context;
  const struct attr_domain *ad_ptr = var_idx < config->attr_domain_count ? config->attr_domains[var_idx] : NULL;
  const void *var = ad_ptr ? ad_ptr->attr_var.data : NULL;
  ERL_NIF_TERM ret = var ? (ERL_NIF_TERM)var : atom_error;
  for (size_t i=0; i<count; i++) {
    size_t index = (size_t)data[i];
    struct sub_info* info = &betree_res->sub_index[index];
    subs[index] = info->sub_id;
    rets[index] = ret;
  }
}

static void update_stats(void *arg, void *data, bool success, const void *context) {
  struct betree_resource* betree_res = (struct betree_resource*) arg;
  size_t index = (size_t)data;

  if (success) {
    // Collect successful matches in subs array
    struct sub_info* info = &betree_res->sub_index[index];
    subs[subs_count] = info->sub_id;
    subs_count++;
  } else if (betree_res->sub_stats != NULL) {
    // Count failures in stats array
    size_t offset = index * betree_res->betree->config->attr_domain_count + (betree_var_t)context;
    atomic_fetch_add(&betree_res->sub_stats[offset], 1);
  }
}

static void bulk_update_stats(void* arg, void** data, size_t count, const void* context) {
  struct betree_resource* betree_res = (struct betree_resource*) arg;
  betree_var_t var_idx = (betree_var_t)context;

  if (betree_res->sub_stats != NULL) {
    for (size_t i = 0; i < count; i++) {
      size_t index = (size_t)data[i];
      size_t offset = index * betree_res->betree->config->attr_domain_count + var_idx;
      atomic_fetch_add(&betree_res->sub_stats[offset], 1);
    }
  }
}

// Group-based stats accumulation structure
struct group_stats_context {
  struct sub_info* sub_index;           // Direct pointer to subscription index
  size_t group_count;            // Number of groups
  betree_var_t* group_results;          // Per-group results: 0=no value, 1=passed, (var_idx+2)=failure reason
  size_t attr_domain_count;             // Number of attribute domains
  ErlNifEnv* env;
  ERL_NIF_TERM* group_ids;             // group_id terms for trace lookup
  struct attr_domain** attr_domains;   // for var name lookup
};

static void update_group_stats(void *arg, void *data, bool success, const void *context) {
  struct group_stats_context* gctx = (struct group_stats_context*) arg;
  size_t index = (size_t)data;
  struct sub_info* info = &gctx->sub_index[index];
  size_t group_idx = info->group_idx;
  bool has_valid_group = group_idx < gctx->group_count;

  if (success) {
    subs[subs_count] = info->sub_id;
    subs_count++;

    if (has_valid_group) {
      gctx->group_results[group_idx] = 1;
    }
  } else {
    bool should_update = has_valid_group && !gctx->group_results[group_idx];

    if (should_update) {
      gctx->group_results[group_idx] = (betree_var_t)context + 2;
    }
  }
}

static void bulk_update_group_stats(void* arg, void** data, size_t count, const void* context) {
  struct group_stats_context* gctx = (struct group_stats_context*) arg;

  for (size_t i = 0; i < count; i++) {
    size_t index = (size_t)data[i];
    struct sub_info* info = &gctx->sub_index[index];
    size_t group_idx = info->group_idx;
    bool has_valid_group = group_idx < gctx->group_count;
    bool should_update = has_valid_group && !gctx->group_results[group_idx];

    if (should_update) {
      gctx->group_results[group_idx] = (betree_var_t)context + 2;
    }
  }
}

static ERL_NIF_TERM nif_betree_search_debug(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM retval;
  struct report *report = NULL;
  size_t pred_index = 0;
  struct betree_event *event = NULL;

  if (argc != 2) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  struct betree_resource *betree_res = get_betree_resource(env, argv[0]);
  if (betree_res == NULL) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }
  struct betree *betree = betree_res->betree;

  unsigned int list_len;
  if (!enif_get_list_length(env, argv[1], &list_len)) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  event = betree_make_event(betree);

  ERL_NIF_TERM head;
  ERL_NIF_TERM tail = argv[1];

  const ERL_NIF_TERM *tuple;
  int tuple_len;

  for (unsigned int i = 0; i < list_len; i++) {
    if (!enif_get_list_cell(env, tail, &head, &tail)) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }

    if (!enif_get_tuple(env, head, &tuple_len, &tuple)) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }

    if (!add_variables(env, betree, event, tuple, tuple_len, pred_index)) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }
    pred_index += (tuple_len - 1);
  }

  // Ensure arrays are allocated and sized correctly
  if (allocated_size < betree_res->sub_count) {
    if (subs != NULL) {
      enif_free(subs);
    }
    if (rets != NULL) {
      enif_free(rets);
      rets = NULL;
    }

    subs = (ERL_NIF_TERM*)enif_alloc(betree_res->sub_count * sizeof(ERL_NIF_TERM));
    rets = (ERL_NIF_TERM*)enif_alloc(betree_res->sub_count * sizeof(ERL_NIF_TERM));
    allocated_size = betree_res->sub_count;

    if (subs == NULL || rets == NULL) {
      // Allocation failed, clean up and return error
      if (subs != NULL) enif_free(subs);
      if (rets != NULL) enif_free(rets);
      subs = rets = NULL;
      allocated_size = 0;
      retval = enif_make_tuple2(env, atom_error, atom_mem_alloc_failed);
      goto cleanup;
    }
  } else if (rets == NULL) {
    // subs is correct size, but rets was freed by betree_search_stats
    rets = (ERL_NIF_TERM*)enif_alloc(allocated_size * sizeof(ERL_NIF_TERM));
    if (rets == NULL) {
      retval = enif_make_tuple2(env, atom_error, atom_mem_alloc_failed);
      goto cleanup;
    }
  }

  report = make_report();
  report->cb = &acc_ret;
  report->cba = &acc_arr;
  report->arg = betree_res;
  bool result = betree_search_with_event(betree, event, report);

  if (result == false) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  retval = enif_make_tuple3(env, atom_ok,
    enif_make_list_from_array(env, subs, betree_res->sub_count),
    enif_make_list_from_array(env, rets, betree_res->sub_count)
  );

cleanup:
  if (event != NULL) {
    betree_free_event(event);
  }
  if (report != NULL) {
    free_report(report);
  }
  return retval;
}

static ERL_NIF_TERM nif_betree_search_stats(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM retval;
  struct report *report = NULL;
  size_t pred_index = 0;
  struct betree_event *event = NULL;
  struct stats_resource *acc = NULL;

  if (argc != 2) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  // Check if first argument is a stats accumulator
  acc = get_stats_accumulator(env, argv[0]);
  struct betree_resource *betree_res = NULL;

  if (acc != NULL) {
    // First argument is a stats accumulator
    if (!acc->active) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }
    betree_res = acc->betree_res;
  } else {
    // First argument should be a betree resource
    betree_res = get_betree_resource(env, argv[0]);
  }
  if (betree_res == NULL) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }
  struct betree *betree = betree_res->betree;

  unsigned int list_len;
  if (!enif_get_list_length(env, argv[1], &list_len)) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  event = betree_make_event(betree);

  ERL_NIF_TERM head;
  ERL_NIF_TERM tail = argv[1];

  const ERL_NIF_TERM *tuple;
  int tuple_len;

  for (unsigned int i = 0; i < list_len; i++) {
    if (!enif_get_list_cell(env, tail, &head, &tail)) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }

    if (!enif_get_tuple(env, head, &tuple_len, &tuple)) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }

    if (!add_variables(env, betree, event, tuple, tuple_len, pred_index)) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }
    pred_index += (tuple_len - 1);
  }

  // Ensure subs array is allocated and sized correctly
  if (allocated_size < betree_res->sub_count) {
    if (subs != NULL) {
      enif_free(subs);
    }
    if (rets != NULL) {
      enif_free(rets);
      rets = NULL;
    }

    subs = (ERL_NIF_TERM*)enif_alloc(betree_res->sub_count * sizeof(ERL_NIF_TERM));
    allocated_size = betree_res->sub_count;

    if (subs == NULL) {
      // Allocation failed, clean up and return error
      subs = NULL;
      allocated_size = 0;
      retval = enif_make_tuple2(env, atom_error, atom_mem_alloc_failed);
      goto cleanup;
    }
  }

  // Reset subs count for this search
  subs_count = 0;

  report = make_report();
  bool result;

  if (acc != NULL) {
    // Set up group-based callbacks
    struct group_stats_context gctx = {
      .sub_index = betree_res->sub_index,
      .group_count = betree_res->group_count,
      .group_results = acc->group_results,
      .attr_domain_count = acc->attr_domain_count,
      .env = env,
      .group_ids = betree_res->group_ids,
      .attr_domains = betree_res->betree->config->attr_domains
    };

    report->cb = &update_group_stats;
    report->cba = &bulk_update_group_stats;
    report->arg = &gctx;

    result = betree_search_with_event(betree, event, report);
  } else {
    // Use regular callbacks
    report->cb = &update_stats;
    report->cba = &bulk_update_stats;
    report->arg = betree_res;

    result = betree_search_with_event(betree, event, report);
  }

  if (result == false) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  retval = enif_make_tuple2(env, atom_ok, enif_make_list_from_array(env, subs, subs_count));

cleanup:
  if (event != NULL) {
    betree_free_event(event);
  }
  if (report != NULL) {
    free_report(report);
  }
  return retval;
}

static ERL_NIF_TERM nif_betree_search_t(ErlNifEnv *env, int argc,
                                        const ERL_NIF_TERM argv[]) {
  if (argc != 3) {
    return enif_make_badarg(env);
  }

  int clock_type = 0;
  if (!enif_get_int(env, argv[2], &clock_type)) {
    return enif_make_badarg(env);
  }
  clock_type = reverse_get_clock_type(clock_type);
  struct timespec start, done;
  clock_gettime(clock_type, &start);
  ERL_NIF_TERM search_res = nif_betree_search(env, argc - 1, argv);
  if (!enif_is_tuple(env, search_res)) {
    return search_res;
  }
  clock_gettime(clock_type, &done);
  ERL_NIF_TERM etspent = make_time(env, &start, &done);
  ERL_NIF_TERM retval = enif_make_tuple2(env, search_res, etspent);
  return retval;
}

static ERL_NIF_TERM nif_betree_search_debug_t(ErlNifEnv *env, int argc,
                                         const ERL_NIF_TERM argv[]) {
  if (argc != 3) {
    return enif_make_badarg(env);
  }

  int clock_type = 0;
  if (!enif_get_int(env, argv[2], &clock_type)) {
    return enif_make_badarg(env);
  }
  clock_type = reverse_get_clock_type(clock_type);
  struct timespec start, done;
  clock_gettime(clock_type, &start);
  ERL_NIF_TERM search_res = nif_betree_search_debug(env, argc - 1, argv);
  if (!enif_is_tuple(env, search_res)) {
    return search_res;
  }
  clock_gettime(clock_type, &done);
  ERL_NIF_TERM etspent = make_time(env, &start, &done);
  ERL_NIF_TERM retval = enif_make_tuple2(env, search_res, etspent);
  return retval;
}

static ERL_NIF_TERM nif_betree_search_stats_t(ErlNifEnv *env, int argc,
                                         const ERL_NIF_TERM argv[]) {
  if (argc != 3) {
    return enif_make_badarg(env);
  }

  int clock_type = 0;
  if (!enif_get_int(env, argv[2], &clock_type)) {
    return enif_make_badarg(env);
  }
  clock_type = reverse_get_clock_type(clock_type);
  struct timespec start, done;
  clock_gettime(clock_type, &start);
  ERL_NIF_TERM search_res = nif_betree_search_stats(env, argc - 1, argv);
  if (!enif_is_tuple(env, search_res)) {
    return search_res;
  }
  clock_gettime(clock_type, &done);
  ERL_NIF_TERM etspent = make_time(env, &start, &done);
  ERL_NIF_TERM retval = enif_make_tuple2(env, search_res, etspent);
  return retval;
}

static ERL_NIF_TERM nif_betree_search_evt(ErlNifEnv *env, int argc,
                                          const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM retval;
  struct report *report = NULL;
  int clock_type = 0;

  if (argc != 3) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  if (!enif_get_int(env, argv[2], &clock_type)) {
    return enif_make_badarg(env);
  }
  clock_type = reverse_get_clock_type(clock_type);
  struct timespec start, done;
  clock_gettime(clock_type, &start);

  struct betree *betree = get_betree(env, argv[0]);
  if (betree == NULL) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  struct evt *evt = get_evt(env, argv[1]);
  if (evt == NULL) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }
  struct betree_event *event = evt->event;

  report = make_report();
  bool result = betree_search_with_event(betree, event, report);

  if (result == false) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  ERL_NIF_TERM res = ids_from_report(env, report);

  retval = enif_make_tuple2(env, atom_ok, res);
cleanup:
  if (report != NULL) {
    free_report(report);
  }
  clock_gettime(clock_type, &done);
  ERL_NIF_TERM etspent = make_time(env, &start, &done);
  return enif_make_tuple2(env, retval, etspent);
}

static ERL_NIF_TERM nif_betree_exists(ErlNifEnv *env, int argc,
                                      const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM retval;
  size_t pred_index = 0;
  struct betree_event *event = NULL;

  if (argc != 2) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  struct betree *betree = get_betree(env, argv[0]);
  if (betree == NULL) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  unsigned int list_len;
  if (!enif_get_list_length(env, argv[1], &list_len)) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  event = betree_make_event(betree);

  ERL_NIF_TERM head;
  ERL_NIF_TERM tail = argv[1];

  const ERL_NIF_TERM *tuple;
  int tuple_len;

  for (unsigned int i = 0; i < list_len; i++) {
    if (!enif_get_list_cell(env, tail, &head, &tail)) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }

    if (!enif_get_tuple(env, head, &tuple_len, &tuple)) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }

    if (!add_variables(env, betree, event, tuple, tuple_len, pred_index)) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }
    pred_index += (tuple_len - 1);
  }

  bool result = betree_exists_with_event(betree, event);

  ERL_NIF_TERM res = result ? atom_true : atom_false;

  retval = enif_make_tuple2(env, atom_ok, res);
cleanup:
  if (event != NULL) {
    betree_free_event(event);
  }
  return retval;
}

static ERL_NIF_TERM betree_write_dot(ErlNifEnv *env, int argc,
                                     const ERL_NIF_TERM argv[]) {
  char file_name[1024];

  if (argc != 2) {
    return enif_make_badarg(env);
  }
  struct betree *betree = get_betree(env, argv[0]);
  if (betree == NULL) {
    return enif_make_badarg(env);
  }
  if (!enif_get_string(env, argv[1], file_name, 1024, ERL_NIF_UTF8)) {
    return enif_make_badarg(env);
  }

  write_dot_to_file(betree, file_name);
  return enif_make_atom(env, "ok");
}

static int cmpfunc(const void *a, const void *b) {
  uint64_t f = *((uint64_t *)a);
  uint64_t s = *((uint64_t *)b);
  if (f > s)
    return 1;
  if (f < s)
    return -1;
  return 0;
}

static ERL_NIF_TERM ids_from_report(ErlNifEnv *env,
                                    const struct report *report) {
  ERL_NIF_TERM res = enif_make_list(env, 0);
  size_t sz = report->matched;
  if (sz > 0) {
    uint64_t *ids = enif_alloc(sz * sizeof(uint64_t));
    if (ids == NULL)
      return res;
    for (size_t i = sz; i;) {
      i--;
      ids[i] = report->subs[i];
    }
    qsort(ids, sz, sizeof(uint64_t), cmpfunc);
    for (size_t i = sz; i;) {
      i--;
      res = enif_make_list_cell(env, enif_make_uint64(env, ids[i]), res);
    }
    enif_free((void *)ids);
  }
  return res;
}


/*static ERL_NIF_TERM nif_betree_delete(ErlNifEnv* env, int argc, const
 * ERL_NIF_TERM argv[])*/
/*{*/
/*ERL_NIF_TERM retval;*/
/*if(argc != 2) {*/
/*retval = enif_make_badarg(env);*/
/*goto cleanup;*/
/*}*/

/*struct betree* betree = get_betree(env, argv[0]);*/

/*betree_sub_t sub_id;*/
/*if(!enif_get_uint64(env, argv[1], &sub_id)) {*/
/*retval = enif_make_badarg(env);*/
/*goto cleanup;*/
/*}*/

/*betree_delete(betree, sub_id);*/
/*retval = atom_ok;*/
/*cleanup:*/

/*return retval;*/
/*}*/

static ERL_NIF_TERM nif_betree_prepare_subs(ErlNifEnv *env, int argc,
                                            const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM retval;
  if (argc != 1) {
    retval = atom_bad_arity;
    goto cleanup;
  }

  struct betree_resource *betree_res = get_betree_resource(env, argv[0]);
  if (betree_res == NULL) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  if (!prepare_subs_data(betree_res)) {
    retval = enif_make_tuple2(env, atom_error, atom_mem_alloc_failed);
    goto cleanup;
  }
  retval = atom_ok;

cleanup:
  return retval;
}

void print_betree(const struct betree* betree);

static ERL_NIF_TERM nif_betree_print(ErlNifEnv *env, int argc,
                                      const ERL_NIF_TERM argv[]) {
  if (argc != 1) {
    return enif_make_badarg(env);
  }

  struct betree *betree = get_betree(env, argv[0]);
  if (betree == NULL) {
    return enif_make_badarg(env);
  }

  print_betree(betree);

  return atom_ok;
}

static ERL_NIF_TERM build_stats_map(ErlNifEnv *env, struct betree_resource *betree_res, bool reset) {
  ERL_NIF_TERM* outer_keys = NULL;
  ERL_NIF_TERM* outer_values = NULL;
  ERL_NIF_TERM* inner_keys = NULL;
  ERL_NIF_TERM* inner_values = NULL;
  ERL_NIF_TERM retval;

  if (betree_res->sub_stats == NULL) {
    // Return empty map if no stats available
    ERL_NIF_TERM empty_map = enif_make_new_map(env);
    return enif_make_tuple2(env, atom_ok, empty_map);
  }

  size_t attr_domain_count = betree_res->betree->config->attr_domain_count;

  // Allocate arrays for maximum possible size
  outer_keys = enif_alloc(betree_res->sub_count * sizeof(ERL_NIF_TERM));
  outer_values = enif_alloc(betree_res->sub_count * sizeof(ERL_NIF_TERM));
  inner_keys = enif_alloc(attr_domain_count * sizeof(ERL_NIF_TERM));
  inner_values = enif_alloc(attr_domain_count * sizeof(ERL_NIF_TERM));

  if (outer_keys == NULL || outer_values == NULL || inner_keys == NULL || inner_values == NULL) {
    retval = enif_make_tuple2(env, atom_error, atom_mem_alloc_failed);
    goto cleanup;
  }

  size_t outer_count = 0;

  for (size_t i = 0; i < betree_res->sub_count; i++) {
    // Count non-zero entries for this subscription
    size_t inner_count = 0;

    for (size_t j = 0; j < attr_domain_count; j++) {
      size_t offset = i * attr_domain_count + j;
      uint64_t count;
      if (reset) {
        // Read and reset counter atomically
        count = atomic_exchange(&betree_res->sub_stats[offset], 0);
      } else {
        // Just read counter
        count = atomic_load(&betree_res->sub_stats[offset]);
      }

      if (count > 0) {
        // Get Var from attr_domain
        const struct attr_domain *ad_ptr = betree_res->betree->config->attr_domains[j];
        inner_keys[inner_count] = ad_ptr ? (ERL_NIF_TERM)ad_ptr->attr_var.data : atom_error;
        inner_values[inner_count] = enif_make_uint64(env, count);
        inner_count++;
      }
    }

    // Skip subscription if no non-zero entries
    if (inner_count == 0) {
      continue;
    }

    // Create inner map from arrays with exact count
    ERL_NIF_TERM inner_map;
    enif_make_map_from_arrays(env, inner_keys, inner_values, inner_count, &inner_map);

    // Add to outer arrays
    outer_keys[outer_count] = betree_res->sub_index[i].sub_id;
    outer_values[outer_count] = inner_map;
    outer_count++;
  }

  // Create outer map from arrays with exact count
  ERL_NIF_TERM result_map;
  enif_make_map_from_arrays(env, outer_keys, outer_values, outer_count, &result_map);
  retval = enif_make_tuple2(env, atom_ok, result_map);

cleanup:
  if (outer_keys) enif_free(outer_keys);
  if (outer_values) enif_free(outer_values);
  if (inner_keys) enif_free(inner_keys);
  if (inner_values) enif_free(inner_values);
  return retval;
}

static ERL_NIF_TERM build_group_stats_map(ErlNifEnv *env, struct betree_resource *betree_res, bool reset) {
  ERL_NIF_TERM* group_keys = NULL;
  ERL_NIF_TERM* group_values = NULL;
  ERL_NIF_TERM* inner_keys = NULL;
  ERL_NIF_TERM* inner_values = NULL;
  ERL_NIF_TERM retval;
  size_t group_count = betree_res->group_count;
  counter_t* groups = betree_res->group_stats;
  size_t attr_count = betree_res->betree->config->attr_domain_count;
  struct attr_domain **attrs = betree_res->betree->config->attr_domains;

  if (groups == NULL || group_count == 0) {
    // Return empty map if no group stats available
    ERL_NIF_TERM empty_map = enif_make_new_map(env);
    return enif_make_tuple2(env, atom_ok, empty_map);
  }

  // Allocate arrays for maximum possible size
  group_keys = enif_alloc(group_count * sizeof(ERL_NIF_TERM));
  group_values = enif_alloc(group_count * sizeof(ERL_NIF_TERM));
  inner_keys = enif_alloc(attr_count * sizeof(ERL_NIF_TERM));
  inner_values = enif_alloc(attr_count * sizeof(ERL_NIF_TERM));

  if (group_keys == NULL || group_values == NULL || inner_keys == NULL || inner_values == NULL) {
    retval = enif_make_tuple2(env, atom_error, atom_mem_alloc_failed);
    goto cleanup;
  }

  size_t outer_count = 0;

  for (size_t group_idx = 0; group_idx < group_count; group_idx++) {
    // Count non-zero entries for this group
    size_t inner_count = 0;

    for (size_t attr_idx = 0; attr_idx < attr_count; attr_idx++) {
      size_t offset = group_idx * attr_count + attr_idx;
      uint64_t count;
      if (reset) {
        // Read and reset counter atomically
        count = atomic_exchange(&groups[offset], 0);
      } else {
        // Just read counter
        count = atomic_load(&groups[offset]);
      }

      if (count > 0) {
        // Get Var from attr_domain
        const struct attr_domain *ad_ptr = attrs[attr_idx];
        inner_keys[inner_count] = ad_ptr ? (ERL_NIF_TERM)ad_ptr->attr_var.data : atom_error;
        inner_values[inner_count] = enif_make_uint64(env, count);
        inner_count++;
      }
    }

    // Skip group if no non-zero entries
    if (inner_count == 0) {
      continue;
    }

    // Create inner map from arrays with exact count
    ERL_NIF_TERM inner_map;
    enif_make_map_from_arrays(env, inner_keys, inner_values, inner_count, &inner_map);

    // Add to outer arrays - get group_id from betree_res global mapping
    group_keys[outer_count] = betree_res->group_ids[group_idx];
    group_values[outer_count] = inner_map;
    outer_count++;
  }

  // Create outer map from arrays with exact count
  ERL_NIF_TERM result_map;
  enif_make_map_from_arrays(env, group_keys, group_values, outer_count, &result_map);
  retval = enif_make_tuple2(env, atom_ok, result_map);

cleanup:
  if (group_keys) enif_free(group_keys);
  if (group_values) enif_free(group_values);
  if (inner_keys) enif_free(inner_keys);
  if (inner_values) enif_free(inner_values);
  return retval;
}

static ERL_NIF_TERM nif_betree_stats(ErlNifEnv *env, int argc,
                                     const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM retval;
  bool group_stats;
  bool reset;

  if (argc != 3) {
    return enif_make_badarg(env);
  }

  // Get betree resource
  struct betree_resource *betree_res = get_betree_resource(env, argv[0]);
  if (betree_res == NULL) {
    return enif_make_badarg(env);
  }

  // Parse group_stats parameter (argv[1])
  if (enif_is_identical(atom_true, argv[1])) {
    group_stats = true;
  } else if (enif_is_identical(atom_false, argv[1])) {
    group_stats = false;
  } else {
    return enif_make_badarg(env);
  }

  // Parse reset parameter (argv[2])
  if (enif_is_identical(atom_true, argv[2])) {
    reset = true;
  } else if (enif_is_identical(atom_false, argv[2])) {
    reset = false;
  } else {
    return enif_make_badarg(env);
  }

  struct timespec start, done;
  clock_gettime(CLOCK_MONOTONIC, &start);

  ERL_NIF_TERM result;
  if (group_stats) {
    // Build group stats map: #{GroupId => #{Var => Count}}
    result = build_group_stats_map(env, betree_res, reset);
  } else {
    // Build regular stats map: #{SubId => #{Var => Count}}
    result = build_stats_map(env, betree_res, reset);
  }

  clock_gettime(CLOCK_MONOTONIC, &done);

  ERL_NIF_TERM etspent = make_time(env, &start, &done);
  retval = enif_make_tuple2(env, result, etspent);
  return retval;
}

static ERL_NIF_TERM nif_betree_stats_start(ErlNifEnv *env, int argc,
                                           const ERL_NIF_TERM argv[]) {
  if (argc != 1) {
    return enif_make_badarg(env);
  }

  struct betree_resource *betree_res = get_betree_resource(env, argv[0]);
  if (betree_res == NULL) {
    return enif_make_badarg(env);
  }

  // Require groups for stats collection
  if (betree_res->group_count == 0) {
    return enif_make_badarg(env);
  }

  struct stats_resource *acc = enif_alloc_resource(MEM_SEARCH_STATS, sizeof(*acc));
  if (acc == NULL) {
    return enif_make_tuple2(env, atom_error, atom_mem_alloc_failed);
  }

  // Initialize stats accumulator
  acc->betree_res = betree_res;
  enif_keep_resource(betree_res);  // Establish dependency on betree_res
  acc->attr_domain_count = betree_res->betree->config->attr_domain_count;
  acc->active = true;

  // Allocate group_results array using actual group count
  size_t group_count = betree_res->group_count;
  acc->group_results = enif_alloc(group_count * sizeof(betree_var_t));

  if (acc->group_results == NULL) {
    enif_release_resource(acc);
    return enif_make_tuple2(env, atom_error, atom_mem_alloc_failed);
  }

  // Initialize group_results to zeros
  memset(acc->group_results, 0, group_count * sizeof(betree_var_t));

  ERL_NIF_TERM acc_term = enif_make_resource(env, acc);
  enif_release_resource(acc);

  return enif_make_tuple2(env, atom_ok, acc_term);
}

static ERL_NIF_TERM build_accumulator_stats_map(ErlNifEnv *env, struct stats_resource *acc) {
  struct betree_resource *betree_res = acc->betree_res;
  size_t group_count = betree_res->group_count;
  size_t attr_count = acc->attr_domain_count;
  struct attr_domain **attrs = betree_res->betree->config->attr_domains;

  // Start with empty map
  ERL_NIF_TERM result_map = enif_make_new_map(env);

  // Process each group in one pass
  for (size_t group_idx = 0; group_idx < group_count; group_idx++) {
    betree_var_t x = acc->group_results[group_idx];

    if (x > 1) {
      // This group had a failure
      betree_var_t attr_idx = x - 2;
      if (attr_idx < attr_count) {
        // Get the attribute term
        const struct attr_domain *ad_ptr = attrs[attr_idx];
        ERL_NIF_TERM attr_term = ad_ptr ? (ERL_NIF_TERM)ad_ptr->attr_var.data : atom_error;
        ERL_NIF_TERM group_id = betree_res->group_ids[group_idx];

        // Check if this attribute already exists in the map
        ERL_NIF_TERM existing_list;
        if (enif_get_map_value(env, result_map, attr_term, &existing_list)) {
          // Attribute exists, prepend the new group to the existing list
          ERL_NIF_TERM new_list = enif_make_list_cell(env, group_id, existing_list);
          enif_make_map_put(env, result_map, attr_term, new_list, &result_map);
        } else {
          // Attribute doesn't exist, create new list with just this group
          ERL_NIF_TERM new_list = enif_make_list1(env, group_id);
          enif_make_map_put(env, result_map, attr_term, new_list, &result_map);
        }
      }
    }
  }

  return enif_make_tuple2(env, atom_ok, result_map);
}

static ERL_NIF_TERM nif_betree_stats_stop(ErlNifEnv *env, int argc,
                                          const ERL_NIF_TERM argv[]) {
  if (argc != 2) {
    return enif_make_badarg(env);
  }

  struct stats_resource *acc = get_stats_accumulator(env, argv[0]);
  if (acc == NULL || !acc->active || acc->group_results == NULL) {
    return enif_make_badarg(env);
  }

  // Parse result parameter (argv[1])
  bool return_stats;
  if (enif_is_identical(atom_true, argv[1])) {
    return_stats = true;
  } else if (enif_is_identical(atom_false, argv[1])) {
    return_stats = false;
  } else {
    return enif_make_badarg(env);
  }

  // Mark accumulator as inactive
  acc->active = false;

  if (return_stats) {
    // Return group statistics map without transferring to betree_res
    return build_accumulator_stats_map(env, acc);
  } else {
    // Transfer statistics to betree_res (original behavior)
    for (size_t group_idx = 0; group_idx < acc->betree_res->group_count; group_idx++) {
      betree_var_t x = acc->group_results[group_idx];
      if (x > 1) {
        size_t offset = group_idx * acc->attr_domain_count + (x - 2);
        atomic_fetch_add(&acc->betree_res->group_stats[offset], 1);
      }
    }
    return atom_ok;
  }
}

// Returns %{group_id => [var_atom]} for all groups, showing which variables
// each group's subscriptions actually use in their boolean expressions.
static ERL_NIF_TERM nif_betree_group_vars(ErlNifEnv *env, int argc,
                                          const ERL_NIF_TERM argv[]) {
  if (argc != 1) {
    return enif_make_badarg(env);
  }

  struct betree_resource *betree_res = get_betree_resource(env, argv[0]);
  if (betree_res == NULL) {
    return enif_make_badarg(env);
  }

  size_t group_count = betree_res->group_count;
  size_t attr_count = betree_res->betree->config->attr_domain_count;
  struct attr_domain **attrs = betree_res->betree->config->attr_domains;

  // Allocate a bitmask per group to accumulate attr_vars union
  size_t words_per_sub = (attr_count + 63) / 64;
  uint64_t *group_masks = enif_alloc(group_count * words_per_sub * sizeof(uint64_t));
  if (group_masks == NULL) {
    return enif_make_tuple2(env, atom_error, atom_mem_alloc_failed);
  }
  memset(group_masks, 0, group_count * words_per_sub * sizeof(uint64_t));

  // Walk all subs and OR their attr_vars into their group's mask
  for (size_t i = 0; i < betree_res->sub_count; i++) {
    size_t group_idx = betree_res->sub_index[i].group_idx;
    if (group_idx >= group_count) continue;
    const struct betree_sub *sub = betree_res->sub_index[i].sub_ptr;
    if (sub == NULL || sub->attr_vars == NULL) continue;

    uint64_t *mask = &group_masks[group_idx * words_per_sub];
    for (size_t w = 0; w < words_per_sub; w++) {
      mask[w] |= sub->attr_vars[w];
    }
  }

  // Build result map: %{group_id => [var_atom]}
  ERL_NIF_TERM result_map = enif_make_new_map(env);

  for (size_t g = 0; g < group_count; g++) {
    uint64_t *mask = &group_masks[g * words_per_sub];
    ERL_NIF_TERM var_list = enif_make_list(env, 0);
    bool has_any = false;

    for (size_t i = 0; i < attr_count; i++) {
      if (test_bit(mask, i)) {
        has_any = true;
        const struct attr_domain *ad = attrs[i];
        ERL_NIF_TERM var_atom = ad ? (ERL_NIF_TERM)ad->attr_var.data : atom_error;
        var_list = enif_make_list_cell(env, var_atom, var_list);
      }
    }

    if (has_any) {
      enif_make_map_put(env, result_map, betree_res->group_ids[g], var_list, &result_map);
    }
  }

  enif_free(group_masks);
  return enif_make_tuple2(env, atom_ok, result_map);
}

// original function descriptors
static ErlNifFunc nif_functions[] = {
    {"betree_print", 1, nif_betree_print, ERL_DIRTY_JOB_IO_BOUND},
    {"betree_make", 1, nif_betree_make, 0},
    {"betree_make", 2, nif_betree_make, 0},
    {"betree_make_event", 3, nif_betree_make_event, 0},
    {"betree_make_sub", 4, nif_betree_make_sub, 0},
    {"betree_insert_sub", 2, nif_betree_insert_sub, 0},
    {"betree_add_sub", 4, nif_betree_add_sub, 0},
    {"betree_exists", 2, nif_betree_exists, 0},
    {"betree_search", 2, nif_betree_search, 0},
    {"betree_search_debug", 2, nif_betree_search_debug, 0},
    {"betree_search_stats", 2, nif_betree_search_stats, 0},
    {"betree_search", 3, nif_betree_search_t, 0},
    {"betree_search_debug", 3, nif_betree_search_debug_t, 0},
    {"betree_search_stats", 3, nif_betree_search_stats_t, 0},
    {"betree_search_evt", 3, nif_betree_search_evt, 0},
    {"betree_write_dot", 2, betree_write_dot, ERL_DIRTY_JOB_IO_BOUND},
    {"betree_prepare_subs", 1, nif_betree_prepare_subs, 0},
    {"betree_stats", 3, nif_betree_stats, ERL_DIRTY_JOB_CPU_BOUND},
    {"betree_add_sub", 5, nif_betree_add_sub, 0},
    {"betree_stats_start", 1, nif_betree_stats_start, 0},
    {"betree_stats_stop", 2, nif_betree_stats_stop, 0},
    {"betree_group_vars", 1, nif_betree_group_vars, 0},
};

// alternative function descriptors with nif_betree_search_'s dirty flag
static ErlNifFunc nif_functions_[] = {
    {"betree_print", 1, nif_betree_print, ERL_DIRTY_JOB_IO_BOUND},
    {"betree_make", 1, nif_betree_make, 0},
    {"betree_make", 2, nif_betree_make, 0},
    {"betree_make_event", 3, nif_betree_make_event, 0},
    {"betree_make_sub", 4, nif_betree_make_sub, 0},
    {"betree_insert_sub", 2, nif_betree_insert_sub, 0},
    {"betree_add_sub", 4, nif_betree_add_sub, 0},
    {"betree_exists", 2, nif_betree_exists, 0},
    {"betree_search", 2, nif_betree_search, 0},
    {"betree_search_debug", 2, nif_betree_search_debug, ERL_DIRTY_JOB_CPU_BOUND},
    {"betree_search_stats", 2, nif_betree_search_stats, ERL_DIRTY_JOB_CPU_BOUND},
    {"betree_search", 3, nif_betree_search_t, 0},
    {"betree_search_debug", 3, nif_betree_search_debug_t, ERL_DIRTY_JOB_CPU_BOUND},
    {"betree_search_stats", 3, nif_betree_search_stats_t, ERL_DIRTY_JOB_CPU_BOUND},
    {"betree_search_evt", 3, nif_betree_search_evt, 0},
    {"betree_write_dot", 2, betree_write_dot, ERL_DIRTY_JOB_IO_BOUND},
    {"betree_prepare_subs", 1, nif_betree_prepare_subs, 0},
    {"betree_stats", 3, nif_betree_stats, ERL_DIRTY_JOB_CPU_BOUND},
    {"betree_add_sub", 5, nif_betree_add_sub, 0},
    {"betree_stats_start", 1, nif_betree_stats_start, 0},
    {"betree_stats_stop", 2, nif_betree_stats_stop, 0},
    {"betree_group_vars", 1, nif_betree_group_vars, 0},
};

// ERL_NIF_INIT replacement for environment-controlled variants
#define ERL_NIF_INIT_(NAME, FUNCS, FUNCS_, LOAD, RELOAD, UPGRADE, UNLOAD) \
ERL_NIF_INIT_PROLOGUE                               \
ERL_NIF_INIT_GLOB                                   \
ERL_NIF_INIT_DECL(NAME);                            \
ERL_NIF_INIT_DECL(NAME)                             \
{                                                   \
    static ErlNifEntry entry =                      \
    {                                               \
        ERL_NIF_MAJOR_VERSION,                      \
        ERL_NIF_MINOR_VERSION,                      \
        #NAME,                                      \
        sizeof(FUNCS) / sizeof(*FUNCS),             \
        FUNCS,                                      \
        LOAD, RELOAD, UPGRADE, UNLOAD,              \
        ERL_NIF_VM_VARIANT,                         \
        1,                                          \
        sizeof(ErlNifResourceTypeInit),             \
        ERL_NIF_MIN_ERTS_VERSION                    \
    };                                              \
    static ErlNifEntry entry_ =                     \
    {                                               \
        ERL_NIF_MAJOR_VERSION,                      \
        ERL_NIF_MINOR_VERSION,                      \
        #NAME,                                      \
        sizeof(FUNCS_) / sizeof(*FUNCS_),           \
        FUNCS_,                                     \
        LOAD, RELOAD, UPGRADE, UNLOAD,              \
        ERL_NIF_VM_VARIANT,                         \
        1,                                          \
        sizeof(ErlNifResourceTypeInit),             \
        ERL_NIF_MIN_ERTS_VERSION                    \
    };                                              \
    ERL_NIF_INIT_BODY;                              \
    {                                               \
        char* selector = getenv("DIRTY_SEARCH");    \
        if (selector != NULL) {                     \
            return &entry_;                         \
        }                                           \
        return &entry;                              \
    }                                               \
}                                                   \
ERL_NIF_INIT_EPILOGUE

ERL_NIF_INIT_(erl_betree_nif, nif_functions, nif_functions_, &load, NULL, NULL, NULL);
