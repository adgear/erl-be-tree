#include <float.h>
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

#include "arraylist.h"
#include "betree.h"
#include "betree_err.h"
#include "debug.h"
#include "hashtable.h"

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
static ErlNifResourceType *MEM_SEARCH_ITERATOR;
static ErlNifResourceType *MEM_SEARCH_STATE;

struct sub {
  const struct betree_sub *sub;
};

struct evt {
  struct betree_event *event;
};

static ERL_NIF_TERM ids_from_report(ErlNifEnv *env,
                                    const struct report *report);
static ERL_NIF_TERM ids_from_report_err(ErlNifEnv *env,
                                        const struct report_err *report);
static ERL_NIF_TERM reasons_from_report_err(ErlNifEnv *env,
                                            const struct report_err *report);
static ERL_NIF_TERM make_time(ErlNifEnv *env, const struct timespec *start,
                              const struct timespec *done);

#include "alloc.h"
#include "hashmap.h"
#include "tree.h"

// search_iterator
struct search_iterator {
  size_t attr_domain_count;
  struct betree_event *event;
  const struct betree_variable **variables;
  size_t undefined_count;
  uint64_t *undefined;
  size_t memoize_count;
  struct memoize memoize;
  struct subs_to_eval subs;
  int node_count;
  struct report_counting *report;
  size_t index;
};

static void search_iterator_init(struct search_iterator *search_iterator) {
  search_iterator->attr_domain_count = 0;
  search_iterator->event = NULL;
  search_iterator->variables = NULL;
  search_iterator->undefined_count = 0;
  search_iterator->undefined = NULL;
  search_iterator->memoize_count = 0;
  search_iterator->memoize.pass = NULL;
  search_iterator->memoize.fail = NULL;
  search_iterator->subs.subs = NULL;
  search_iterator->subs.capacity = 0;
  search_iterator->subs.count = 0;
  search_iterator->report = NULL;
  search_iterator->node_count = 0;
  search_iterator->index = 0;
}

static void search_iterator_deinit(struct search_iterator *search_iterator) {
  if (search_iterator->event != NULL) {
    betree_free_event(search_iterator->event);
    search_iterator->event = NULL;
  }
  if (search_iterator->variables != NULL) {
    search_iterator->attr_domain_count = 0;
    bfree(search_iterator->variables);
    search_iterator->variables = NULL;
  }
  if (search_iterator->undefined != NULL) {
    search_iterator->undefined_count = 0;
    bfree(search_iterator->undefined);
    search_iterator->undefined = NULL;
  }
  search_iterator->memoize_count = 0;
  if (search_iterator->memoize.pass != NULL) {
    bfree(search_iterator->memoize.pass);
    search_iterator->memoize.pass = NULL;
  }
  if (search_iterator->memoize.fail != NULL) {
    bfree(search_iterator->memoize.fail);
    search_iterator->memoize.fail = NULL;
  }
  if (search_iterator->subs.subs != NULL) {
    search_iterator->subs.capacity = 0;
    search_iterator->subs.count = 0;
    bfree(search_iterator->subs.subs);
    search_iterator->subs.subs = NULL;
  }
  if (search_iterator->report != NULL) {
    free_report_counting(search_iterator->report);
    search_iterator->report = NULL;
  }
  search_iterator->node_count = 0;
}
// search_iterator. End

// search_state
struct search_state {
  size_t attr_domain_count;
  const struct betree_variable **variables;
  uint64_t *undefined;
  struct memoize memoize;
  struct subs_to_eval subs;
  struct report *report;
  size_t index;
  struct timespec start;
  struct timespec current;
};

static void search_state_init(struct search_state *search_state) {
  search_state->attr_domain_count = 0;
  search_state->variables = NULL;
  search_state->undefined = NULL;
  search_state->memoize.pass = NULL;
  search_state->memoize.fail = NULL;
  search_state->subs.subs = NULL;
  search_state->subs.capacity = 0;
  search_state->subs.count = 0;
  search_state->report = NULL;
  search_state->index = 0;
  search_state->start.tv_sec = 0;
  search_state->start.tv_nsec = 0;
  search_state->current.tv_sec = 0;
  search_state->current.tv_nsec = 0;
}

static void search_state_deinit(struct search_state *search_state) {
  if (search_state->variables != NULL) {
    search_state->attr_domain_count = 0;
    bfree(search_state->variables);
    search_state->variables = NULL;
  }
  if (search_state->undefined != NULL) {
    bfree(search_state->undefined);
    search_state->undefined = NULL;
  }
  free_memoize(search_state->memoize);
  if (search_state->subs.subs != NULL) {
    search_state->subs.capacity = 0;
    search_state->subs.count = 0;
    bfree(search_state->subs.subs);
    search_state->subs.subs = NULL;
  }
  if (search_state->report != NULL) {
    free_report(search_state->report);
    search_state->report = NULL;
  }
}

static ERL_NIF_TERM
save_search_state(ErlNifEnv *env, size_t attr_domain_count,
                  const struct betree_variable **variables, uint64_t *undefined,
                  struct memoize *memoize, struct subs_to_eval *subs,
                  struct report *report, size_t index, struct timespec *start,
                  struct timespec *current) {
  struct search_state *search_state =
      enif_alloc_resource(MEM_SEARCH_STATE, sizeof(*search_state));
  search_state_init(search_state);

  ERL_NIF_TERM search_state_term = enif_make_resource(env, search_state);
  enif_release_resource(search_state);

  search_state->attr_domain_count = attr_domain_count;
  search_state->variables = variables;

  search_state->undefined = undefined;

  search_state->memoize = *memoize;

  search_state->subs = *subs;

  search_state->report = report;

  search_state->index = index;

  search_state->start = *start;
  search_state->current = *current;

  return search_state_term;
}
// search_state. End

static ERL_NIF_TERM make_atom(ErlNifEnv *env, const char *name) {
  ERL_NIF_TERM ret;

  if (enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1)) {
    return ret;
  }

  return enif_make_atom(env, name);
}

static void cleanup_betree(ErlNifEnv *env, void *obj) {
  (void)env;
  struct betree *betree = obj;
  betree_deinit(betree);
}

static void cleanup_event(ErlNifEnv *env, void *obj) {
  (void)env;
  struct evt *evt = obj;
  betree_free_event(evt->event);
  evt->event = NULL;
}

static void cleanup_search_iterator(ErlNifEnv *env, void *obj) {
  (void)env;
  struct search_iterator *search_iterator = obj;
  search_iterator_deinit(search_iterator);
}

static void cleanup_search_state(ErlNifEnv *env, void *obj) {
  (void)env;
  struct search_state *search_state = obj;
  search_state_deinit(search_state);
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
  MEM_SEARCH_ITERATOR = enif_open_resource_type(
      env, NULL, "search_iterator", cleanup_search_iterator, flags, NULL);
  if (MEM_SEARCH_ITERATOR == NULL) {
    return -1;
  }
  MEM_SEARCH_STATE = enif_open_resource_type(env, NULL, "search_state",
                                             cleanup_search_state, flags, NULL);
  if (MEM_SEARCH_STATE == NULL) {
    return -1;
  }

  return 0;
}

static struct betree *get_betree(ErlNifEnv *env, const ERL_NIF_TERM term) {
  struct betree *betree = NULL;
  enif_get_resource(env, term, MEM_BETREE, (void *)&betree);
  return betree;
}

static struct evt *get_evt(ErlNifEnv *env, const ERL_NIF_TERM term) {
  struct evt *evt = NULL;
  enif_get_resource(env, term, MEM_EVENT, (void *)&evt);
  return evt;
}

static struct search_iterator *get_search_iterator(ErlNifEnv *env,
                                                   const ERL_NIF_TERM term) {
  struct search_iterator *search_iterator = NULL;
  enif_get_resource(env, term, MEM_SEARCH_ITERATOR, (void *)&search_iterator);
  return search_iterator;
}

static struct search_state *get_search_state(ErlNifEnv *env,
                                             const ERL_NIF_TERM term) {
  struct search_state *search_state = NULL;
  enif_get_resource(env, term, MEM_SEARCH_STATE, (void *)&search_state);
  return search_state;
}

static struct sub *get_sub(ErlNifEnv *env, const ERL_NIF_TERM term) {
  struct sub *sub = NULL;
  enif_get_resource(env, term, MEM_SUB, (void *)&sub);
  return sub;
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

static bool add_domains(ErlNifEnv *env, struct betree *betree,
                        ERL_NIF_TERM list, unsigned int list_len) {
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
      betree_add_integer_variable(betree, domain_name, allow_undefined, min,
                                  max);
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
      betree_add_integer_list_variable(betree, domain_name, allow_undefined,
                                       min, max);
    } else if (enif_is_identical(atom_int_enum, tuple[1])) {
      size_t max = SIZE_MAX;
      if (tuple_len == 4) {
        uint64_t u64_max;
        if (!enif_get_uint64(env, tuple[3], &u64_max)) {
          return false;
        }
        max = (size_t)u64_max;
      }
      betree_add_integer_enum_variable(betree, domain_name, allow_undefined,
                                       max);
    } else if (enif_is_identical(atom_bin, tuple[1])) {
      size_t max = SIZE_MAX;
      if (tuple_len == 4) {
        uint64_t u64_max;
        if (!enif_get_uint64(env, tuple[3], &u64_max)) {
          return false;
        }
        max = (size_t)u64_max;
      }
      betree_add_string_variable(betree, domain_name, allow_undefined, max);
    } else if (enif_is_identical(atom_bin_list, tuple[1])) {
      size_t max = SIZE_MAX;
      if (tuple_len == 4) {
        uint64_t u64_max;
        if (!enif_get_uint64(env, tuple[3], &u64_max)) {
          return false;
        }
        max = (size_t)u64_max;
      }
      betree_add_string_list_variable(betree, domain_name, allow_undefined,
                                      max);
    } else if (enif_is_identical(atom_bool, tuple[1])) {
      betree_add_boolean_variable(betree, domain_name, allow_undefined);
    } else if (enif_is_identical(atom_float, tuple[1])) {
      double min = -DBL_MAX;
      double max = DBL_MAX;
      if (tuple_len == 5) {
        if (!enif_get_double(env, tuple[3], &min) ||
            !enif_get_double(env, tuple[4], &max)) {
          return false;
        }
      }
      betree_add_float_variable(betree, domain_name, allow_undefined, min, max);
    } else if (enif_is_identical(atom_frequency_caps, tuple[1])) {
      betree_add_frequency_caps_variable(betree, domain_name, allow_undefined);
    } else if (enif_is_identical(atom_segments, tuple[1])) {
      betree_add_segments_variable(betree, domain_name, allow_undefined);
    } else {
      return false;
    }
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
  if (argc != 1) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  struct betree *betree = enif_alloc_resource(MEM_BETREE, sizeof(*betree));
  betree_init(betree);

  ERL_NIF_TERM term = enif_make_resource(env, betree);

  enif_release_resource(betree);

  unsigned int list_len;
  if (!enif_get_list_length(env, argv[0], &list_len)) {
    retval = enif_make_badarg(env);
    goto cleanup;
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

    if (!add_domains(env, betree, head, inner_list_len)) {
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

static ERL_NIF_TERM nif_betree_search_evt_ids(ErlNifEnv *env, int argc,
                                              const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM retval;
  struct report *report = NULL;
  uint64_t *ids = NULL;
  int clock_type = 0;

  if (argc != 4) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  if (!enif_get_int(env, argv[3], &clock_type)) {
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

  unsigned int sz;
  if (!enif_get_list_length(env, argv[2], &sz) || sz == 0) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  ERL_NIF_TERM head;
  ERL_NIF_TERM tail = argv[2];
  ids = enif_alloc(sz * sizeof(uint64_t));

  for (unsigned int i = 0; i < sz; i++) {
    if (!enif_get_list_cell(env, tail, &head, &tail)) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }

    if (!enif_get_uint64(env, head, &ids[i])) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }
  }

  report = make_report();
  bool result = betree_search_with_event_ids(betree, event, report, ids, sz);

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
  if (ids != NULL) {
    enif_free((void *)ids);
  }

  clock_gettime(clock_type, &done);
  ERL_NIF_TERM etspent = make_time(env, &start, &done);
  return enif_make_tuple2(env, retval, etspent);
}

static ERL_NIF_TERM nif_betree_search_ids(ErlNifEnv *env, int argc,
                                          const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM retval;
  struct report *report = NULL;
  size_t pred_index = 0;
  struct betree_event *event = NULL;
  uint64_t *ids = NULL;
  int clock_type = 0;

  if (argc != 4) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  if (!enif_get_int(env, argv[3], &clock_type)) {
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

  unsigned int sz;
  if (!enif_get_list_length(env, argv[2], &sz) || sz == 0) {
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

  tail = argv[2];
  ids = enif_alloc(sz * sizeof(uint64_t));

  for (unsigned int i = 0; i < sz; i++) {
    if (!enif_get_list_cell(env, tail, &head, &tail)) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }

    if (!enif_get_uint64(env, head, &ids[i])) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }
  }

  report = make_report();
  bool result = betree_search_with_event_ids(betree, event, report, ids, sz);

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
  if (ids != NULL) {
    enif_free((void *)ids);
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

// Convert to microseconds
static ERL_NIF_TERM make_time(ErlNifEnv *env, const struct timespec *start,
                              const struct timespec *done) {
  ErlNifSInt64 tspent = (done->tv_sec - start->tv_sec) * 1000000 +
                        (done->tv_nsec - start->tv_nsec) / 1000;
  ERL_NIF_TERM etspent = enif_make_int64(env, tspent);
  return etspent;
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

static ERL_NIF_TERM ids_from_report_err(ErlNifEnv *env,
                                        const struct report_err *report) {
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

static ERL_NIF_TERM reasons_from_report_err(ErlNifEnv *env,
                                            const struct report_err *report) {
  hashtable *reasons = report->reason_sub_id_list;
  ERL_NIF_TERM res = enif_make_list(env, 0);
  size_t res_size = reasons->size;
  if (res_size > 0) {
    for (size_t idx = 0; idx < reasons->capacity && res_size > 0; idx++) {
      if (reasons->body[idx].key && reasons->body[idx].value) {
        size_t sz = ((arraylist *)(reasons->body[idx].value))->size;
        if (sz > 0) {
          ERL_NIF_TERM sub_res = enif_make_list(env, 0);
          uint64_t *ids = enif_alloc(sz * sizeof(uint64_t));
          if (ids == NULL)
            continue;
          for (size_t i = sz; i;) {
            i--;
            ids[i] = ((arraylist *)(reasons->body[idx].value))->body[i];
          }
          qsort(ids, sz, sizeof(uint64_t), cmpfunc);
          for (size_t i = sz; i;) {
            i--;
            sub_res = enif_make_list_cell(env, enif_make_uint64(env, ids[i]),
                                          sub_res);
          }
          enif_free((void *)ids);
          ERL_NIF_TERM res_single_key_list = enif_make_tuple2(
              env, enif_make_atom(env, reasons->body[idx].key), sub_res);
          res = enif_make_list_cell(env, res_single_key_list, res);
          res_size--;
        }
      }
    }
  }
  return res;
}

static void bump_used_reductions(ErlNifEnv *env, int count) {
  const int reduction_per_count = 1;
  const int DEFAULT_ERLANG_REDUCTION_COUNT = 2000;
  int reductions_used = count * reduction_per_count;
  int pct_used = 100 * reductions_used / DEFAULT_ERLANG_REDUCTION_COUNT;
  if (pct_used > 0) {
    if (pct_used > 100) {
      pct_used = 100;
    }
  } else {
    pct_used = 1;
  }
  enif_consume_timeslice(env, pct_used);
}

static ERL_NIF_TERM nif_betree_search_iterator(ErlNifEnv *env, int argc,
                                               const ERL_NIF_TERM argv[]) {
  if (argc != 2) {
    return enif_make_badarg(env);
  }

  struct betree *betree = get_betree(env, argv[0]);
  if (betree == NULL) {
    return enif_make_badarg(env);
  }

  unsigned int list_len;
  if (!enif_get_list_length(env, argv[1], &list_len)) {
    return enif_make_badarg(env);
  }

  struct search_iterator *search_iterator =
      enif_alloc_resource(MEM_SEARCH_ITERATOR, sizeof(*search_iterator));
  search_iterator_init(search_iterator);

  ERL_NIF_TERM search_iterator_term = enif_make_resource(env, search_iterator);
  enif_release_resource(search_iterator);

  search_iterator->attr_domain_count = betree->config->attr_domain_count;
  search_iterator->event = betree_make_event(betree);

  ERL_NIF_TERM head;
  ERL_NIF_TERM tail = argv[1];

  size_t pred_index = 0;
  const ERL_NIF_TERM *tuple;
  int tuple_len;

  for (unsigned int i = 0; i < list_len; i++) {
    if (!enif_get_list_cell(env, tail, &head, &tail)) {
      search_iterator_deinit(search_iterator);
      return enif_make_badarg(env);
    }

    if (!enif_get_tuple(env, head, &tuple_len, &tuple)) {
      search_iterator_deinit(search_iterator);
      return enif_make_badarg(env);
    }

    if (!add_variables(env, betree, search_iterator->event, tuple, tuple_len,
                       pred_index)) {
      search_iterator_deinit(search_iterator);
      return enif_make_badarg(env);
    }
    pred_index += (tuple_len - 1);
  }

  fill_event(betree->config, search_iterator->event);
  sort_event_lists(search_iterator->event);
  search_iterator->variables = make_environment(
      betree->config->attr_domain_count, search_iterator->event);
  if (validate_variables(betree->config, search_iterator->variables) == false) {
    fprintf(stderr, "Failed to validate event\n");
    search_iterator_deinit(search_iterator);
    return enif_make_badarg(env);
  }

  search_iterator->undefined = make_undefined_with_count(
      betree->config->attr_domain_count, search_iterator->variables,
      &search_iterator->undefined_count);
  search_iterator->memoize = make_memoize_with_count(
      betree->config->pred_map->memoize_count, &search_iterator->memoize_count);
  init_subs_to_eval_ext(&search_iterator->subs, 64);
  match_be_tree_node_counting(
      (const struct attr_domain **)betree->config->attr_domains,
      search_iterator->variables, betree->cnode, &search_iterator->subs,
      &search_iterator->node_count);
  bump_used_reductions(env, search_iterator->node_count);

  ERL_NIF_TERM subs_count = enif_make_ulong(env, search_iterator->subs.count);
  ERL_NIF_TERM node_count = enif_make_long(env, search_iterator->node_count);
  ERL_NIF_TERM ret =
      enif_make_tuple3(env, search_iterator_term, subs_count, node_count);

  return enif_make_tuple2(env, atom_ok, ret);
}

static ERL_NIF_TERM nif_betree_search_next(ErlNifEnv *env, int argc,
                                           const ERL_NIF_TERM argv[]) {
  if (argc != 1) {
    return enif_make_badarg(env);
  }

  struct search_iterator *search_iterator = get_search_iterator(env, argv[0]);
  if (search_iterator == NULL) {
    return enif_make_badarg(env);
  }

  if (search_iterator->report == NULL) {
    search_iterator->report = make_report_counting();
  }

  if (search_iterator->index >= search_iterator->subs.count) {
    ERL_NIF_TERM matched;
    if (search_iterator->report->matched == 0) {
      ERL_NIF_TERM tmp[1];
      matched = enif_make_list_from_array(env, tmp, 0);
    } else {
      ERL_NIF_TERM *arr =
          enif_alloc(sizeof(ERL_NIF_TERM) * search_iterator->report->matched);
      for (int i = 0; i < search_iterator->report->matched; ++i) {
        arr[i] = enif_make_uint64(env, search_iterator->report->subs[i]);
      }
      matched =
          enif_make_list_from_array(env, arr, search_iterator->report->matched);
      enif_free(arr);
    }
    bump_used_reductions(env, 1);
    return enif_make_tuple2(env, atom_ok, matched);
  }

  const struct betree_sub *sub =
      search_iterator->subs.subs[search_iterator->index];
  search_iterator->report->evaluated++;
  int count_before =
      search_iterator->report->node_count + search_iterator->report->ops_count;
  bool id_matched = false;
  if (match_sub_counting(search_iterator->attr_domain_count,
                         search_iterator->variables, sub,
                         search_iterator->report, &search_iterator->memoize,
                         search_iterator->undefined) == true) {
    id_matched = true;
    add_sub_counting(sub->id, search_iterator->report);
  }
  search_iterator->index++;
  int count_diff = (search_iterator->report->node_count +
                    search_iterator->report->ops_count) -
                   count_before;
  bump_used_reductions(env, count_diff);

  if (search_iterator->index == search_iterator->subs.count) {
    ERL_NIF_TERM matched;
    if (search_iterator->report->matched == 0) {
      ERL_NIF_TERM tmp[1];
      matched = enif_make_list_from_array(env, tmp, 0);
    } else {
      ERL_NIF_TERM *arr =
          enif_alloc(sizeof(ERL_NIF_TERM) * search_iterator->report->matched);
      for (int i = 0; i < search_iterator->report->matched; ++i) {
        arr[i] = enif_make_uint64(env, search_iterator->report->subs[i]);
      }
      matched =
          enif_make_list_from_array(env, arr, search_iterator->report->matched);
      enif_free(arr);
    }
    return enif_make_tuple2(env, atom_ok, matched);
  }

  ERL_NIF_TERM id[1];
  ERL_NIF_TERM matched;
  if (id_matched) {
    id[0] = enif_make_uint64(env, sub->id);
    matched = enif_make_list_from_array(env, id, 1);
    return enif_make_tuple2(env, atom_continue, matched);
  }
  matched = enif_make_list_from_array(env, id, 0);
  return enif_make_tuple2(env, atom_continue, matched);
}

static ERL_NIF_TERM nif_betree_search_all(ErlNifEnv *env, int argc,
                                          const ERL_NIF_TERM argv[]) {
  if (argc != 1) {
    return enif_make_badarg(env);
  }

  struct search_iterator *search_iterator = get_search_iterator(env, argv[0]);
  if (search_iterator == NULL) {
    return enif_make_badarg(env);
  }

  search_iterator->report = make_report_counting();
  for (size_t i = 0; i < search_iterator->subs.count; i++) {
    const struct betree_sub *sub = search_iterator->subs.subs[i];
    search_iterator->report->evaluated++;
    if (match_sub_counting(search_iterator->attr_domain_count,
                           search_iterator->variables, sub,
                           search_iterator->report, &search_iterator->memoize,
                           search_iterator->undefined) == true) {
      add_sub_counting(sub->id, search_iterator->report);
    }
  }

  ERL_NIF_TERM matched;
  if (search_iterator->report->matched == 0) {
    ERL_NIF_TERM tmp[1];
    matched = enif_make_list_from_array(env, tmp, 0);
  } else {
    ERL_NIF_TERM *arr =
        enif_alloc(sizeof(ERL_NIF_TERM) * search_iterator->report->matched);
    for (int i = 0; i < search_iterator->report->matched; ++i) {
      arr[i] = enif_make_uint64(env, search_iterator->report->subs[i]);
    }
    matched =
        enif_make_list_from_array(env, arr, search_iterator->report->matched);
    enif_free(arr);
  }

  bump_used_reductions(env, search_iterator->report->node_count +
                                search_iterator->report->ops_count);

  return enif_make_tuple2(env, atom_ok, matched);
}

static ERL_NIF_TERM
nif_betree_search_iterator_release(ErlNifEnv *env, int argc,
                                   const ERL_NIF_TERM argv[]) {
  if (argc != 1) {
    return enif_make_badarg(env);
  }

  struct search_iterator *search_iterator = get_search_iterator(env, argv[0]);
  if (search_iterator == NULL) {
    return enif_make_badarg(env);
  }

  search_iterator_deinit(search_iterator);

  return atom_ok;
}

static void bump_used_reductions_time_based(ErlNifEnv *env, ErlNifTime duration,
                                            int threshold) {
  if (threshold <= 0) {
    enif_consume_timeslice(env, 100);
    return;
  }
  int pct_used = duration * 100 / threshold;
  if (pct_used > 0) {
    if (pct_used > 100) {
      pct_used = 100;
    }
  } else {
    pct_used = 1;
  }
  enif_consume_timeslice(env, pct_used);
}

static ERL_NIF_TERM nif_betree_search_yield(ErlNifEnv *env, int argc,
                                            const ERL_NIF_TERM argv[]) {
  if (argc != 4) {
    return enif_make_badarg(env);
  }

  int clock_type;
  if (!enif_get_int(env, argv[2], &clock_type)) {
    return enif_make_badarg(env);
  }
  clock_type = reverse_get_clock_type(clock_type);
  struct timespec start, done;
  clock_gettime(clock_type, &start);

  ErlNifTime timeslice_start = enif_monotonic_time(ERL_NIF_USEC);

  int yield_threshold_microseconds;
  if (!enif_get_int(env, argv[3], &yield_threshold_microseconds)) {
    return enif_make_badarg(env);
  }

  struct betree *betree = get_betree(env, argv[0]);
  if (betree == NULL) {
    return enif_make_badarg(env);
  }

  struct evt *evt = get_evt(env, argv[1]);
  if (evt == NULL) {
    return enif_make_badarg(env);
  }
  struct betree_event *event = evt->event;

  fill_event(betree->config, event);
  sort_event_lists(event);
  const struct betree_variable **variables =
      make_environment(betree->config->attr_domain_count, event);
  if (validate_variables(betree->config, variables) == false) {
    fprintf(stderr, "Failed to validate event\n");
    return enif_make_badarg(env);
  }

  struct subs_to_eval subs;
  init_subs_to_eval_ext(&subs, 1024);
  match_be_tree((const struct attr_domain **)betree->config->attr_domains,
                variables, betree->cnode, &subs);
  if (subs.count == 0) {
    ERL_NIF_TERM tmp[1];
    ERL_NIF_TERM matched = enif_make_list_from_array(env, tmp, 0);

    clock_gettime(clock_type, &done);
    ERL_NIF_TERM elapsed = make_time(env, &start, &done);
    ERL_NIF_TERM ret = enif_make_tuple2(env, atom_ok, matched);

    ErlNifTime timeslice_checkpoint = enif_monotonic_time(ERL_NIF_USEC);
    bump_used_reductions_time_based(env, timeslice_checkpoint - timeslice_start,
                                    yield_threshold_microseconds);

    return enif_make_tuple2(env, ret, elapsed);
  }

  uint64_t *undefined =
      make_undefined(betree->config->attr_domain_count, variables);
  struct memoize memoize =
      make_memoize(betree->config->pred_map->memoize_count);

  struct report *report = make_report();
  size_t index = 0;
  while (index < subs.count) {
    ErlNifTime timeslice_checkpoint = enif_monotonic_time(ERL_NIF_USEC);
    if (timeslice_checkpoint - timeslice_start >=
        yield_threshold_microseconds) {
      clock_gettime(clock_type, &done);
      ERL_NIF_TERM search_state = save_search_state(
          env, betree->config->attr_domain_count, variables, undefined,
          &memoize, &subs, report, index, &start, &done);
      ERL_NIF_TERM elapsed = make_time(env, &start, &done);
      ERL_NIF_TERM ret = enif_make_tuple2(env, atom_continue, search_state);
      enif_consume_timeslice(env, 100);
      return enif_make_tuple2(env, ret, elapsed);
    }

    const struct betree_sub *sub = subs.subs[index];
    report->evaluated++;
    if (match_sub(betree->config->attr_domain_count, variables, sub, report,
                  &memoize, undefined) == true) {
      add_sub(sub->id, report);
    }

    ++index;
  }

  ERL_NIF_TERM matched = ids_from_report(env, report);

  bfree(variables);
  bfree(subs.subs);
  bfree(undefined);
  free_memoize(memoize);
  free_report(report);

  clock_gettime(clock_type, &done);
  ERL_NIF_TERM elapsed = make_time(env, &start, &done);
  ERL_NIF_TERM ret = enif_make_tuple2(env, atom_ok, matched);

  ErlNifTime timeslice_checkpoint = enif_monotonic_time(ERL_NIF_USEC);
  bump_used_reductions_time_based(env, timeslice_checkpoint - timeslice_start,
                                  yield_threshold_microseconds);

  return enif_make_tuple2(env, ret, elapsed);
}

static ERL_NIF_TERM nif_betree_search_next_yield(ErlNifEnv *env, int argc,
                                                 const ERL_NIF_TERM argv[]) {
  if (argc != 3) {
    return enif_make_badarg(env);
  }

  int clock_type;
  if (!enif_get_int(env, argv[1], &clock_type)) {
    return enif_make_badarg(env);
  }
  clock_type = reverse_get_clock_type(clock_type);
  struct timespec start, done;
  clock_gettime(clock_type, &start);

  ErlNifTime timeslice_start = enif_monotonic_time(ERL_NIF_USEC);

  int yield_threshold_microseconds;
  if (!enif_get_int(env, argv[2], &yield_threshold_microseconds)) {
    return enif_make_badarg(env);
  }

  struct search_state *search_state = get_search_state(env, argv[0]);
  if (search_state == NULL) {
    return enif_make_badarg(env);
  }

  while (search_state->index < search_state->subs.count) {

    const struct betree_sub *sub = search_state->subs.subs[search_state->index];
    search_state->report->evaluated++;
    if (match_sub(search_state->attr_domain_count, search_state->variables, sub,
                  search_state->report, &search_state->memoize,
                  search_state->undefined) == true) {
      add_sub(sub->id, search_state->report);
    }

    search_state->index++;

    if (search_state->index < search_state->subs.count) {
      ErlNifTime timeslice_checkpoint = enif_monotonic_time(ERL_NIF_USEC);
      if (timeslice_checkpoint - timeslice_start >=
          yield_threshold_microseconds) {
        clock_gettime(clock_type, &search_state->current);
        ERL_NIF_TERM elapsed = make_time(env, &start, &search_state->current);
        ERL_NIF_TERM ret = enif_make_tuple2(env, atom_continue, argv[0]);
        enif_consume_timeslice(env, 100);
        return enif_make_tuple2(env, ret, elapsed);
      }
    }
  }

  ERL_NIF_TERM matched = ids_from_report(env, search_state->report);

  ERL_NIF_TERM ret = enif_make_tuple2(env, atom_ok, matched);
  clock_gettime(clock_type, &done);
  ERL_NIF_TERM elapsed = make_time(env, &search_state->start, &done);

  ErlNifTime timeslice_checkpoint = enif_monotonic_time(ERL_NIF_USEC);
  bump_used_reductions_time_based(env, timeslice_checkpoint - timeslice_start,
                                  yield_threshold_microseconds);

  return enif_make_tuple2(env, ret, elapsed);
}

static ERL_NIF_TERM nif_betree_search_ids_yield(ErlNifEnv *env, int argc,
                                                const ERL_NIF_TERM argv[]) {
  if (argc != 5) {
    return enif_make_badarg(env);
  }

  int clock_type;
  if (!enif_get_int(env, argv[3], &clock_type)) {
    return enif_make_badarg(env);
  }
  clock_type = reverse_get_clock_type(clock_type);
  struct timespec start, done;
  clock_gettime(clock_type, &start);

  ErlNifTime timeslice_start = enif_monotonic_time(ERL_NIF_USEC);

  int yield_threshold_microseconds;
  if (!enif_get_int(env, argv[4], &yield_threshold_microseconds)) {
    return enif_make_badarg(env);
  }

  struct betree *betree = get_betree(env, argv[0]);
  if (betree == NULL) {
    return enif_make_badarg(env);
  }

  struct evt *evt = get_evt(env, argv[1]);
  if (evt == NULL) {
    return enif_make_badarg(env);
  }
  struct betree_event *event = evt->event;

  unsigned int len;
  if (!enif_get_list_length(env, argv[2], &len) || len == 0) {
    return enif_make_badarg(env);
  }

  ERL_NIF_TERM head;
  ERL_NIF_TERM tail = argv[2];
  uint64_t *ids = enif_alloc(len * sizeof(uint64_t));
  if (ids == NULL) {
    return enif_make_badarg(env);
  }

  for (unsigned int i = 0; i < len; i++) {
    if (!enif_get_list_cell(env, tail, &head, &tail)) {
      enif_free((void *)ids);
      return enif_make_badarg(env);
    }

    if (!enif_get_uint64(env, head, &ids[i])) {
      enif_free((void *)ids);
      return enif_make_badarg(env);
    }
  }

  struct report *report = make_report();
  bool result = betree_search_with_event_ids(betree, event, report, ids, len);

  if (result == false) {
    free_report(report);
    enif_free((void *)ids);
    return enif_make_badarg(env);
  }

  ERL_NIF_TERM matched = ids_from_report(env, report);
  free_report(report);
  enif_free((void *)ids);

  ERL_NIF_TERM ret = enif_make_tuple2(env, atom_ok, matched);
  clock_gettime(clock_type, &done);
  ERL_NIF_TERM elapsed = make_time(env, &start, &done);

  ErlNifTime timeslice_checkpoint = enif_monotonic_time(ERL_NIF_USEC);
  bump_used_reductions_time_based(env, timeslice_checkpoint - timeslice_start,
                                  yield_threshold_microseconds);

  return enif_make_tuple2(env, ret, elapsed);
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


/* 
 * betree search error reason code begin
 */
static struct betree *get_betree_err(ErlNifEnv *env, const ERL_NIF_TERM term) {
  struct betree_err *betree = NULL;
  enif_get_resource(env, term, MEM_BETREE, (void *)&betree);
  return betree;
}

static bool add_domains_err(ErlNifEnv *env, struct betree_err *betree,
                            ERL_NIF_TERM list, unsigned int list_len) {
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
      betree_add_integer_variable_err(betree, domain_name, allow_undefined, min,
                                      max);
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
      betree_add_integer_list_variable_err(betree, domain_name, allow_undefined,
                                           min, max);
    } else if (enif_is_identical(atom_int_enum, tuple[1])) {
      size_t max = SIZE_MAX;
      if (tuple_len == 4) {
        uint64_t u64_max;
        if (!enif_get_uint64(env, tuple[3], &u64_max)) {
          return false;
        }
        max = (size_t)u64_max;
      }
      betree_add_integer_enum_variable_err(betree, domain_name, allow_undefined,
                                           max);
    } else if (enif_is_identical(atom_bin, tuple[1])) {
      size_t max = SIZE_MAX;
      if (tuple_len == 4) {
        uint64_t u64_max;
        if (!enif_get_uint64(env, tuple[3], &u64_max)) {
          return false;
        }
        max = (size_t)u64_max;
      }
      betree_add_string_variable_err(betree, domain_name, allow_undefined, max);
    } else if (enif_is_identical(atom_bin_list, tuple[1])) {
      size_t max = SIZE_MAX;
      if (tuple_len == 4) {
        uint64_t u64_max;
        if (!enif_get_uint64(env, tuple[3], &u64_max)) {
          return false;
        }
        max = (size_t)u64_max;
      }
      betree_add_string_list_variable_err(betree, domain_name, allow_undefined,
                                          max);
    } else if (enif_is_identical(atom_bool, tuple[1])) {
      betree_add_boolean_variable_err(betree, domain_name, allow_undefined);
    } else if (enif_is_identical(atom_float, tuple[1])) {
      double min = -DBL_MAX;
      double max = DBL_MAX;
      if (tuple_len == 5) {
        if (!enif_get_double(env, tuple[3], &min) ||
            !enif_get_double(env, tuple[4], &max)) {
          return false;
        }
      }
      betree_add_float_variable_err(betree, domain_name, allow_undefined, min,
                                    max);
    } else if (enif_is_identical(atom_frequency_caps, tuple[1])) {
      betree_add_frequency_caps_variable_err(betree, domain_name,
                                             allow_undefined);
    } else if (enif_is_identical(atom_segments, tuple[1])) {
      betree_add_segments_variable_err(betree, domain_name, allow_undefined);
    } else {
      return false;
    }
  }
  return true;
}

static bool add_variables_err(ErlNifEnv *env, struct betree_err *betree,
                              struct betree_event *event,
                              const ERL_NIF_TERM *tuple, int tuple_len,
                              size_t initial_domain_index) {
  // Start at 1 to not use the record name
  for (int i = 1; i < tuple_len; i++) {
    ERL_NIF_TERM element = tuple[i];
    if (enif_is_identical(atom_undefined, element)) {
      continue;
    }
    size_t domain_index = initial_domain_index + i - 1;
    struct betree_variable_definition def =
        betree_get_variable_definition_err(betree, domain_index);
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

static ERL_NIF_TERM nif_betree_make_sub_ids(ErlNifEnv *env, int argc,
                                            const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM retval;
  if (argc != 1) {
    retval = atom_bad_arity;
    goto cleanup;
  }

  struct betree_err *betree = get_betree_err(env, argv[0]);

  bool res = betree_make_sub_ids(betree);
  if (res == false) {
    retval = atom_failed;
    goto cleanup;
  }

  retval = atom_ok;
cleanup:
  return retval;
}

static ERL_NIF_TERM nif_betree_make_err(ErlNifEnv *env, int argc,
                                        const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM retval;
  if (argc != 1) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  struct betree_err *betree = enif_alloc_resource(MEM_BETREE, sizeof(*betree));
  betree_init_err(betree);

  ERL_NIF_TERM term = enif_make_resource(env, betree);

  enif_release_resource(betree);

  unsigned int list_len;
  if (!enif_get_list_length(env, argv[0], &list_len)) {
    retval = enif_make_badarg(env);
    goto cleanup;
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

    if (!add_domains_err(env, betree, head, inner_list_len)) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }
  }

  retval = enif_make_tuple(env, 2, atom_ok, term);
cleanup:
  return retval;
}

static ERL_NIF_TERM nif_betree_make_event_err(ErlNifEnv *env, int argc,
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

  struct betree_err *betree = get_betree_err(env, argv[0]);
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

    if (!add_variables_err(env, betree, event, tuple, tuple_len, pred_index)) {
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

static ERL_NIF_TERM nif_betree_make_sub_err(ErlNifEnv *env, int argc,
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

  struct betree_err *betree = get_betree_err(env, argv[0]);

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
      betree_make_sub_err(betree, sub_id, constant_count,
                          (const struct betree_constant **)constants, expr);
  if (betree_sub == NULL) {
    retval = enif_make_tuple2(env, atom_error, atom_failed);
    goto cleanup;
  }

  struct sub *sub = enif_alloc_resource(MEM_SUB, sizeof(*sub));
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

static ERL_NIF_TERM nif_betree_insert_sub_err(ErlNifEnv *env, int argc,
                                              const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM retval;

  if (argc != 2) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  struct betree_err *betree = get_betree_err(env, argv[0]);
  if (betree == NULL) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  struct sub *sub = get_sub(env, argv[1]);
  if (sub == NULL) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  bool result = betree_insert_sub_err(betree, sub->sub);
  if (result) {
    retval = atom_ok;
  } else {
    retval = atom_error;
  }
cleanup:
  return retval;
}

static ERL_NIF_TERM nif_betree_search_err(ErlNifEnv *env, int argc,
                                          const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM retval;
  struct report_err *report = NULL;
  size_t pred_index = 0;
  struct betree_event *event = NULL;

  if (argc != 2) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  struct betree_err *betree = get_betree_err(env, argv[0]);
  if (betree == NULL) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  unsigned int list_len;
  if (!enif_get_list_length(env, argv[1], &list_len)) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  event = betree_make_event_err(betree);

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

    if (!add_variables_err(env, betree, event, tuple, tuple_len, pred_index)) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }
    pred_index += (tuple_len - 1);
  }

  report = make_report_err();
  bool result = betree_search_with_event_err(betree, event, report);

  ERL_NIF_TERM ret = (result) ? atom_ok : atom_error;
  ERL_NIF_TERM res = ids_from_report_err(env, report);
  ERL_NIF_TERM res_reason = reasons_from_report_err(env, report);

  retval = enif_make_tuple3(env, ret, res, res_reason);
cleanup:
  if (event != NULL) {
    betree_free_event(event);
  }
  if (report != NULL) {
    free_report_err(report);
  }
  return retval;
}

static ERL_NIF_TERM nif_betree_search_t_err(ErlNifEnv *env, int argc,
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
  ERL_NIF_TERM search_res = nif_betree_search_err(env, argc - 1, argv);
  if (!enif_is_tuple(env, search_res)) {
    return search_res;
  }
  clock_gettime(clock_type, &done);
  ERL_NIF_TERM etspent = make_time(env, &start, &done);
  ERL_NIF_TERM retval = enif_make_tuple2(env, search_res, etspent);
  return retval;
}

static ERL_NIF_TERM nif_betree_search_evt_err(ErlNifEnv *env, int argc,
                                              const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM retval;
  struct report_err *report = NULL;
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

  struct betree_err *betree = get_betree_err(env, argv[0]);
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

  report = make_report_err();
  bool result = betree_search_with_event_err(betree, event, report);

  ERL_NIF_TERM ret = (result) ? atom_ok : atom_error;
  ERL_NIF_TERM res = ids_from_report_err(env, report);
  ERL_NIF_TERM res_reason = reasons_from_report_err(env, report);

  retval = enif_make_tuple3(env, ret, res, res_reason);
cleanup:
  if (report != NULL) {
    free_report_err(report);
  }
  clock_gettime(clock_type, &done);
  ERL_NIF_TERM etspent = make_time(env, &start, &done);
  return enif_make_tuple2(env, retval, etspent);
}

static ERL_NIF_TERM nif_betree_search_evt_ids_err(ErlNifEnv *env, int argc,
                                                  const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM retval;
  struct report_err *report = NULL;
  uint64_t *ids = NULL;
  int clock_type = 0;

  if (argc != 4) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  if (!enif_get_int(env, argv[3], &clock_type)) {
    return enif_make_badarg(env);
  }
  clock_type = reverse_get_clock_type(clock_type);
  struct timespec start, done;
  clock_gettime(clock_type, &start);

  struct betree_err *betree = get_betree_err(env, argv[0]);
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

  unsigned int sz;
  if (!enif_get_list_length(env, argv[2], &sz) || sz == 0) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  ERL_NIF_TERM head;
  ERL_NIF_TERM tail = argv[2];
  ids = enif_alloc(sz * sizeof(uint64_t));

  for (unsigned int i = 0; i < sz; i++) {
    if (!enif_get_list_cell(env, tail, &head, &tail)) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }

    if (!enif_get_uint64(env, head, &ids[i])) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }
  }

  report = make_report_err();
  bool result =
      betree_search_with_event_ids_err(betree, event, report, ids, sz);

  ERL_NIF_TERM ret = (result) ? atom_ok : atom_error;
  ERL_NIF_TERM res = ids_from_report_err(env, report);
  ERL_NIF_TERM res_reason = reasons_from_report_err(env, report);

  retval = enif_make_tuple3(env, ret, res, res_reason);
cleanup:
  if (report != NULL) {
    free_report_err(report);
  }
  if (ids != NULL) {
    enif_free((void *)ids);
  }

  clock_gettime(clock_type, &done);
  ERL_NIF_TERM etspent = make_time(env, &start, &done);
  return enif_make_tuple2(env, retval, etspent);
}

static ERL_NIF_TERM nif_betree_search_ids_err(ErlNifEnv *env, int argc,
                                              const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM retval;
  struct report_err *report = NULL;
  size_t pred_index = 0;
  struct betree_event *event = NULL;
  uint64_t *ids = NULL;
  int clock_type = 0;

  if (argc != 4) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  if (!enif_get_int(env, argv[3], &clock_type)) {
    return enif_make_badarg(env);
  }
  clock_type = reverse_get_clock_type(clock_type);
  struct timespec start, done;
  clock_gettime(clock_type, &start);

  struct betree_err *betree = get_betree_err(env, argv[0]);
  if (betree == NULL) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  unsigned int list_len;
  if (!enif_get_list_length(env, argv[1], &list_len)) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  unsigned int sz;
  if (!enif_get_list_length(env, argv[2], &sz) || sz == 0) {
    retval = enif_make_badarg(env);
    goto cleanup;
  }

  event = betree_make_event_err(betree);

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

  tail = argv[2];
  ids = enif_alloc(sz * sizeof(uint64_t));

  for (unsigned int i = 0; i < sz; i++) {
    if (!enif_get_list_cell(env, tail, &head, &tail)) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }

    if (!enif_get_uint64(env, head, &ids[i])) {
      retval = enif_make_badarg(env);
      goto cleanup;
    }
  }

  report = make_report_err();
  bool result =
      betree_search_with_event_ids_err(betree, event, report, ids, sz);

  ERL_NIF_TERM ret = (result) ? atom_ok : atom_error;
  ERL_NIF_TERM res = ids_from_report_err(env, report);
  ERL_NIF_TERM res_reason = reasons_from_report_err(env, report);

  retval = enif_make_tuple3(env, ret, res, res_reason);
cleanup:
  if (event != NULL) {
    betree_free_event(event);
  }
  if (report != NULL) {
    free_report_err(report);
  }
  if (ids != NULL) {
    enif_free((void *)ids);
  }

  clock_gettime(clock_type, &done);
  ERL_NIF_TERM etspent = make_time(env, &start, &done);
  return enif_make_tuple2(env, retval, etspent);
}

static ERL_NIF_TERM betree_write_dot_err(ErlNifEnv *env, int argc,
                                         const ERL_NIF_TERM argv[]) {
  char file_name[1024];

  if (argc != 2) {
    return enif_make_badarg(env);
  }
  struct betree_err *betree = get_betree_err(env, argv[0]);
  if (betree == NULL) {
    return enif_make_badarg(env);
  }
  if (!enif_get_string(env, argv[1], file_name, 1024, ERL_NIF_UTF8)) {
    return enif_make_badarg(env);
  }

  write_dot_to_file_err(betree, file_name);
  return enif_make_atom(env, "ok");
}
/* 
 * betree search error reason code end
 */

static ErlNifFunc nif_functions[] = {
    {"betree_make", 1, nif_betree_make, 0},
    {"betree_make_event", 3, nif_betree_make_event, 0},
    {"betree_make_sub", 4, nif_betree_make_sub, 0},
    {"betree_insert_sub", 2, nif_betree_insert_sub, 0},
    {"betree_exists", 2, nif_betree_exists, 0},
    {"betree_search", 2, nif_betree_search, 0},
    {"betree_search", 3, nif_betree_search_t, 0},
    {"betree_search_evt", 3, nif_betree_search_evt, 0},
    {"betree_search_evt", 4, nif_betree_search_evt_ids, 0},
    {"betree_search_ids", 4, nif_betree_search_ids, 0},
    {"betree_write_dot", 2, betree_write_dot, ERL_DIRTY_JOB_IO_BOUND},
    {"search_iterator", 2, nif_betree_search_iterator, 0},
    {"search_next", 1, nif_betree_search_next, 0},
    {"search_all", 1, nif_betree_search_all, 0},
    {"search_iterator_release", 1, nif_betree_search_iterator_release, 0},
    {"search_yield", 4, nif_betree_search_yield, 0},
    {"search_next_yield", 3, nif_betree_search_next_yield, 0},
    {"search_ids_yield", 5, nif_betree_search_ids_yield, 0},
    {"betree_make_sub_ids", 1, nif_betree_make_sub_ids, 0},
    {"betree_make_err", 1, nif_betree_make_err, 0},
    {"betree_make_event_err", 3, nif_betree_make_event_err, 0},
    {"betree_make_sub_err", 4, nif_betree_make_sub_err, 0},
    {"betree_insert_sub_err", 2, nif_betree_insert_sub_err, 0},
    {"betree_search_err", 2, nif_betree_search_err, 0},
    {"betree_search_err", 3, nif_betree_search_t_err, 0},
    {"betree_search_evt_err", 3, nif_betree_search_evt_err, 0},
    {"betree_search_evt_err", 4, nif_betree_search_evt_ids_err, 0},
    {"betree_search_ids_err", 4, nif_betree_search_ids_err, 0},
    {"betree_write_dot_err", 2, betree_write_dot_err, ERL_DIRTY_JOB_IO_BOUND},
};

ERL_NIF_INIT(erl_betree_nif, nif_functions, &load, NULL, NULL, NULL);
