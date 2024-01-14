#include <float.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "erl_nif.h"

#include "betree.h"

// return values
static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_ok_continuation;
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

static ErlNifResourceType* MEM_BETREE;
static ErlNifResourceType* MEM_SUB;
static ErlNifResourceType* MEM_TRAPPED_SEARCH;

struct sub {
    const struct betree_sub* sub;
};

static ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM ret;

    if(enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1)) {
        return ret;
    }

    return enif_make_atom(env, name);
}

static void cleanup_betree(ErlNifEnv* env, void* obj)
{
    (void)env;
    struct betree* betree = obj;
    betree_deinit(betree);
}

typedef struct {
    struct betree* betree;
    struct betree_event* event;
} trapped_search_t;

static void trapped_search_dtor(ErlNifEnv* env, void* obj)
{
    (void)env;
    if (obj == NULL)
    {
        return;
    }
    trapped_search_t* trapped_search = obj;
    if (trapped_search->event != NULL)
    {
        betree_free_event(trapped_search->event);
        trapped_search->event = NULL;
    }

    if (trapped_search->betree != NULL)
    {
        // trapped_state->betree clean up as "betree" resource type,
        // see MEM_BETREE = enif_open_resource_type(...) in load function.
        // Thus, betree_deinit(trapped_state->betree) is not needed here.
        trapped_search->betree = NULL;
    }
}

static int load(ErlNifEnv* env, void **priv_data, ERL_NIF_TERM load_info)
{
    (void)priv_data;
    (void)load_info;

    atom_ok = make_atom(env, "ok");
    atom_ok_continuation = make_atom(env, "ok_continuation");
    atom_error = make_atom(env, "error");
    atom_bad_expr = make_atom(env, "bad_expr");
    atom_bad_event = make_atom(env, "bad_event");
    atom_bad_arity = make_atom(env, "bad_arity");
    atom_bad_id = make_atom(env, "bad_id");
    atom_bad_constant_list = make_atom(env, "bad_constant_list");
    atom_bad_constant = make_atom(env, "bad_constant");
    atom_bad_binary = make_atom(env, "bad_binary");
    atom_failed = make_atom(env, "failed");

    atom_int = make_atom(env, "int");;
    atom_int_list = make_atom(env, "int_list");
    atom_int_enum = make_atom(env, "int_enum");
    atom_bin = make_atom(env, "bin");
    atom_bin_list = make_atom(env, "bin_list");
    atom_bool = make_atom(env, "bool");;
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

    int flags = (int)((unsigned)ERL_NIF_RT_CREATE | (unsigned)ERL_NIF_RT_TAKEOVER);
    MEM_BETREE = enif_open_resource_type(env, NULL, "betree", cleanup_betree, flags, NULL);
    if(MEM_BETREE == NULL) {
        return -1;
    }
    // We don't own the betree_sub, betree will deinit it. No dtor
    MEM_SUB = enif_open_resource_type(env, NULL, "sub", NULL, flags, NULL);
    if(MEM_SUB == NULL) {
        return -1;
    }

    MEM_TRAPPED_SEARCH = enif_open_resource_type(env,
                                         NULL,
                                         "trapped_search",
                                         trapped_search_dtor,
                                         ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER,
                                         NULL);
    if(MEM_TRAPPED_SEARCH == NULL) {
        return -1;
    }

    return 0;
}

static struct betree* get_betree(ErlNifEnv* env, const ERL_NIF_TERM term)
{
    struct betree* betree = NULL;
    enif_get_resource(env, term, MEM_BETREE, (void*)&betree);
    return betree;
}

static struct sub* get_sub(ErlNifEnv* env, const ERL_NIF_TERM term)
{
    struct sub* sub = NULL;
    enif_get_resource(env, term, MEM_SUB, (void*)&sub);
    return sub;
}

static char *alloc_string(ErlNifBinary bin)
{
    size_t key_len = bin.size;
    char *key = enif_alloc((key_len + 1) * sizeof(*key));
    if (!key) {
        return NULL;
    }

    memcpy(key, bin.data, key_len);

    key[key_len] = 0;

    return key;
}

#define DOMAIN_NAME_LEN 256

static bool add_domains(ErlNifEnv* env, struct betree* betree, ERL_NIF_TERM list, unsigned int list_len)
{
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail = list;
    const ERL_NIF_TERM* tuple;
    int tuple_len;
    char domain_name[DOMAIN_NAME_LEN];
    bool allow_undefined;
    for(unsigned int i = 0; i < list_len; i++) {
        if(!enif_get_list_cell(env, tail, &head, &tail)) {
            return false;
        }

        if(!enif_get_tuple(env, head, &tuple_len, &tuple)) {
            return false;
        }
        if(tuple_len < 3) {
            return false;
        }

        if(!enif_get_atom(env, tuple[0], domain_name, DOMAIN_NAME_LEN, ERL_NIF_LATIN1)) {
            return false;
        }

        if(enif_is_identical(atom_allow_undefined, tuple[2])) {
            allow_undefined = true;
        }
        else if(enif_is_identical(atom_disallow_undefined, tuple[2])) {
            allow_undefined = false;
        }
        else {
            return false;
        }

        if(enif_is_identical(atom_int, tuple[1]) || enif_is_identical(atom_int64, tuple[1])) {
            int64_t min = INT64_MIN;
            int64_t max = INT64_MAX;
            if(tuple_len == 5) {
                if(!enif_get_int64(env, tuple[3], &min) || !enif_get_int64(env, tuple[4], &max)) {
                    return false;
                }
            }
            betree_add_integer_variable(betree, domain_name, allow_undefined, min, max);
        }
        else if(enif_is_identical(atom_int_list, tuple[1])) {
            int64_t min = INT64_MIN;
            int64_t max = INT64_MAX;
            if(tuple_len == 5) {
                int64_t min, max;
                if(!enif_get_int64(env, tuple[3], &min) || !enif_get_int64(env, tuple[4], &max)) {
                    return false;
                }
            }
            betree_add_integer_list_variable(betree, domain_name, allow_undefined, min, max);
        }
        else if(enif_is_identical(atom_int_enum, tuple[1])) {
            size_t max = SIZE_MAX;
            if(tuple_len == 4) {
                uint64_t u64_max;
                if(!enif_get_uint64(env, tuple[3], &u64_max)) {
                    return false;
                }
                max = (size_t)u64_max;
            }
            betree_add_integer_enum_variable(betree, domain_name, allow_undefined, max);
        }
        else if(enif_is_identical(atom_bin, tuple[1])) {
            size_t max = SIZE_MAX;
            if(tuple_len == 4) {
                uint64_t u64_max;
                if(!enif_get_uint64(env, tuple[3], &u64_max)) {
                    return false;
                }
                max = (size_t)u64_max;
            }
            betree_add_string_variable(betree, domain_name, allow_undefined, max);
        }
        else if(enif_is_identical(atom_bin_list, tuple[1])) {
            size_t max = SIZE_MAX;
            if(tuple_len == 4) {
                uint64_t u64_max;
                if(!enif_get_uint64(env, tuple[3], &u64_max)) {
                    return false;
                }
                max = (size_t)u64_max;
            }
            betree_add_string_list_variable(betree, domain_name, allow_undefined, max);
        }
        else if(enif_is_identical(atom_bool, tuple[1])) {
            betree_add_boolean_variable(betree, domain_name, allow_undefined);
        }
        else if(enif_is_identical(atom_float, tuple[1])) {
            double min = -DBL_MAX;
            double max = DBL_MAX;
            if(tuple_len == 5) {
                if(!enif_get_double(env, tuple[3], &min) || !enif_get_double(env, tuple[4], &max)) {
                    return false;
                }
            }
            betree_add_float_variable(betree, domain_name, allow_undefined, min, max);
        }
        else if(enif_is_identical(atom_frequency_caps, tuple[1])) {
            betree_add_frequency_caps_variable(betree, domain_name, allow_undefined);
        }
        else if(enif_is_identical(atom_segments, tuple[1])) {
            betree_add_segments_variable(betree, domain_name, allow_undefined);
        }
        else {
            return false;
        }
    }
    return true;
}

static ERL_NIF_TERM nif_betree_make(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM retval;
    if(argc != 1) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    struct betree* betree = enif_alloc_resource(MEM_BETREE, sizeof(*betree));
    betree_init(betree);

    ERL_NIF_TERM term = enif_make_resource(env, betree);

    enif_release_resource(betree);

    unsigned int list_len;
    if(!enif_get_list_length(env, argv[0], &list_len)) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    ERL_NIF_TERM head;
    ERL_NIF_TERM tail = argv[0];

    for(unsigned int i = 0; i < list_len; i++) {
        if(!enif_get_list_cell(env, tail, &head, &tail)) {
            return false;
        }

        unsigned int inner_list_len;
        if(!enif_get_list_length(env, head, &inner_list_len)) {
            retval = enif_make_badarg(env);
            goto cleanup;
        }

        if(!add_domains(env, betree, head, inner_list_len)) {
            retval = enif_make_badarg(env);
            goto cleanup;
        }
    }

    retval = enif_make_tuple(env, 2, atom_ok, term);
cleanup:
    return retval;
}

#define CONSTANT_NAME_LEN 256

static ERL_NIF_TERM nif_betree_make_sub(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM retval;
    ErlNifBinary bin;
    char* expr = NULL;
    size_t constant_count = 0;
    struct betree_constant** constants = NULL;
    if(argc != 4) {
        retval = enif_make_tuple2(env, atom_error, atom_bad_arity);
        goto cleanup;
    }

    struct betree* betree = get_betree(env, argv[0]);

    betree_sub_t sub_id;
    if(!enif_get_uint64(env, argv[1], &sub_id)) {
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
        if(!enif_get_list_cell(env, tail, &head, &tail)) {
            retval = enif_make_tuple2(env, atom_error, atom_bad_constant_list);
            goto cleanup;
        }
        const ERL_NIF_TERM* tuple;
        int tuple_len;

        if(!enif_get_tuple(env, head, &tuple_len, &tuple)) {
            retval = enif_make_tuple4(env, atom_error, atom_bad_constant, enif_make_int64(env, i), atom_unknown);
            goto cleanup;
        }

        if(tuple_len != 2) {
            retval = enif_make_tuple4(env, atom_error, atom_bad_constant, enif_make_int64(env, i), atom_unknown);
            goto cleanup;
        }
        char constant_name[CONSTANT_NAME_LEN];
        if(!enif_get_atom(env, tuple[0], constant_name, CONSTANT_NAME_LEN, ERL_NIF_LATIN1)) {
            retval = enif_make_tuple4(env, atom_error, atom_bad_constant, enif_make_int64(env, i), atom_unknown);
            goto cleanup;
        }

        int64_t value;
        if(!enif_get_int64(env, tuple[1], &value)) {
            retval = enif_make_tuple4(env, atom_error, atom_bad_constant, enif_make_int64(env, i), make_atom(env, constant_name));
            goto cleanup;
        }
        constants[i] = betree_make_integer_constant(constant_name, value);
    }

    if(!enif_inspect_iolist_as_binary(env, argv[3], &bin)) {
        retval = enif_make_tuple2(env, atom_error, atom_bad_binary);
        goto cleanup;
    }
    expr = alloc_string(bin);
    if (expr == NULL) {
        retval = enif_make_tuple2(env, atom_error, atom_bad_binary);
        goto cleanup;
    }

    const struct betree_sub* betree_sub = betree_make_sub(betree, sub_id, constant_count, (const struct betree_constant**)constants, expr);
    if(betree_sub == NULL) {
        retval = enif_make_tuple2(env, atom_error, atom_failed);
        goto cleanup;
    }

    struct sub* sub = enif_alloc_resource(MEM_SUB, sizeof(*sub));
    sub->sub = betree_sub;

    ERL_NIF_TERM sub_term = enif_make_resource(env, sub);

    enif_release_resource(sub);

    retval = enif_make_tuple(env, 2, atom_ok, sub_term);
cleanup:
    if(expr != NULL) {
        enif_free(expr);
    }
    if(constants != NULL) {
        betree_free_constants(constant_count, constants);
        enif_free(constants);
    }

    return retval;
}

static ERL_NIF_TERM nif_betree_insert_sub(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM retval;

    if(argc != 2) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    struct betree* betree = get_betree(env, argv[0]);
    if(betree == NULL) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    struct sub* sub = get_sub(env, argv[1]);
    if(sub == NULL) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    bool result = betree_insert_sub(betree, sub->sub);
    if(result) {
        retval = atom_ok;
    }
    else {
        retval = atom_error;
    }
cleanup:
    return retval;
}

static bool get_binary(ErlNifEnv* env, ERL_NIF_TERM term, const char* name, struct betree_variable** variable)
{
    ErlNifBinary bin;

    if (!enif_inspect_binary(env, term, &bin)) {
        return false;
    }

    char* value = alloc_string(bin);

    *variable = betree_make_string_variable(name, value);

    enif_free(value);

    return true;
}

static bool get_boolean(ErlNifEnv* env, ERL_NIF_TERM term, const char* name, struct betree_variable** variable) 
{
    (void)env;
    bool value = false;
    if(enif_is_identical(atom_true, term)) {
        value = true;
    }
    else if(enif_is_identical(atom_false, term)) {
        value = false;
    }
    else {
        return false;
    }

    *variable = betree_make_boolean_variable(name, value);

    return true;
}

static bool get_int(ErlNifEnv* env, ERL_NIF_TERM term, const char* name, struct betree_variable** variable) 
{
    int64_t value;
    if(!enif_get_int64(env, term, &value)) {
        return false;
    }

    *variable = betree_make_integer_variable(name, value);

    return true;
}

static bool get_float(ErlNifEnv* env, ERL_NIF_TERM term, const char* name, struct betree_variable** variable) 
{
    double value;
    if(!enif_get_double(env, term, &value)) {
        return false;
    }

    *variable = betree_make_float_variable(name, value);

    return true;
}

static bool get_bin_list(ErlNifEnv* env, ERL_NIF_TERM term, const char* name, struct betree_variable** variable) 
{
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail = term;
    ErlNifBinary bin;
    const char* value;
    unsigned int length;

    if (!enif_get_list_length(env, term, &length)) {
        return false;
    }

    struct betree_string_list* list = betree_make_string_list(length);

    for (unsigned int i = 0; i < length; i++) {
        if(!enif_get_list_cell(env, tail, &head, &tail)) {
            betree_free_string_list(list);
            return false;
        }

        if(!enif_inspect_binary(env, head, &bin)) {
            betree_free_string_list(list);
            return false;
        }

        value = alloc_string(bin);
        betree_add_string(list, i, value);
        enif_free((char*)value);
    }

    *variable = betree_make_string_list_variable(name, list);

    return true;
}

static bool get_int_list(ErlNifEnv* env, ERL_NIF_TERM term, const char* name, struct betree_variable** variable)
{
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail = term;
    int64_t value;
    unsigned int length;

    if (!enif_get_list_length(env, term, &length)) {
        return false;
    }

    struct betree_integer_list* list = betree_make_integer_list(length);

    for (unsigned int i = 0; i < length; i++) {
        if(!enif_get_list_cell(env, tail, &head, &tail)) {
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

static bool get_frequency_cap(ErlNifEnv* env, ERL_NIF_TERM term, struct betree_frequency_cap **ptr) 
{
    int cap_arity;
    int key_arity;
    ErlNifBinary bin;
    char* type_str = NULL;
    char* ns_str = NULL;
    bool success = true;

    const ERL_NIF_TERM *cap_content;
    const ERL_NIF_TERM *key_content;

    if (!enif_get_tuple(env, term, &cap_arity, &cap_content) || cap_arity != 3) {
        return false;
    }

    if (!enif_get_tuple(env, cap_content[0], &key_arity, &key_content) || key_arity != 3) {
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
    if(enif_is_identical(atom_undefined, cap_content[2])) {
        timestamp_defined = false;
    }
    else if (enif_get_int64(env, cap_content[2], &timestamp)) {
        timestamp_defined = true;
    }
    else {
        success = false;
        goto cleanup;
    }
    struct betree_frequency_cap* frequency_cap = betree_make_frequency_cap(type_str, id, ns_str, timestamp_defined, timestamp, value);
    if(frequency_cap == NULL) {
        success = false;
        goto cleanup;
    }
    *ptr = frequency_cap;
cleanup:
    if(type_str != NULL) {
        enif_free(type_str);
    }
    if(ns_str != NULL) {
        enif_free(ns_str);
    }

    return success;
}

static bool get_frequency_caps_list(ErlNifEnv* env, ERL_NIF_TERM term, const char* name, struct betree_variable** variable) 
{
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail = term;
    unsigned int length;

    if (!enif_get_list_length(env, term, &length)) {
        return false;
    }

    struct betree_frequency_caps* list = betree_make_frequency_caps(length);

    for (unsigned int i = 0; i < length; i++) {
        if(!enif_get_list_cell(env, tail, &head, &tail)) {
            betree_free_frequency_caps(list);
            return false;
        }
        struct betree_frequency_cap* frequency_cap;
        if(!get_frequency_cap(env, head, &frequency_cap)) {
            betree_free_frequency_caps(list);
            return false;
        }
        betree_add_frequency_cap(list, i, frequency_cap);
    }

    *variable = betree_make_frequency_caps_variable(name, list);

    return true;
}

static bool get_segment(ErlNifEnv* env, ERL_NIF_TERM term, struct betree_segment **ptr)
{
    int segment_arity;
    const ERL_NIF_TERM *segment_content;

    if (!enif_get_tuple(env, term, &segment_arity, &segment_content) || segment_arity != 2) {
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

static bool get_segments_list(ErlNifEnv* env, ERL_NIF_TERM term, const char* name, struct betree_variable** variable) 
{
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail = term;
    unsigned int length;

    if (!enif_get_list_length(env, term, &length)) {
        return false;
    }

    struct betree_segments* list = betree_make_segments(length);

    for (unsigned int i = 0; i < length; i++) {
        if(!enif_get_list_cell(env, tail, &head, &tail)) {
            betree_free_segments(list);
            return false;
        }
        struct betree_segment* segment;
        if(!get_segment(env, head, &segment)) {
            betree_free_segments(list);
            return false;
        }

        betree_add_segment(list, i, segment);
    }

    *variable = betree_make_segments_variable(name, list);

    return true;
}

static bool add_variables(ErlNifEnv* env, struct betree* betree, struct betree_event* event, const ERL_NIF_TERM* tuple, int tuple_len, size_t initial_domain_index)
{
    // Start at 1 to not use the record name
    for(int i = 1; i < tuple_len; i++) {
        ERL_NIF_TERM element = tuple[i];
        if(enif_is_identical(atom_undefined, element)) {
            continue;
        }
        size_t domain_index = initial_domain_index + i - 1;
        struct betree_variable_definition def = betree_get_variable_definition(betree, domain_index);
        bool result;
        struct betree_variable* variable = NULL;
        switch(def.type) {
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
        if(result == false) {
            if(variable != NULL) {
                betree_free_variable(variable);
            }
            return false;
        }
        betree_set_variable(event, domain_index, variable);
    }
    return true;
}

static ERL_NIF_TERM nif_betree_search(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM retval;
    struct report* report = NULL;
    size_t pred_index = 0;
    struct betree_event* event = NULL;

    if(argc != 2) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    struct betree* betree = get_betree(env, argv[0]);
    if(betree == NULL) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    unsigned int list_len;
    if(!enif_get_list_length(env, argv[1], &list_len)) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    event = betree_make_event(betree);

    ERL_NIF_TERM head;
    ERL_NIF_TERM tail = argv[1];

    const ERL_NIF_TERM* tuple;
    int tuple_len;

    for(unsigned int i = 0; i < list_len; i++) {
        if(!enif_get_list_cell(env, tail, &head, &tail)) {
            retval = enif_make_badarg(env);
            goto cleanup;
        }

        if(!enif_get_tuple(env, head, &tuple_len, &tuple)) {
            retval = enif_make_badarg(env);
            goto cleanup;
        }

        if(!add_variables(env, betree, event, tuple, tuple_len, pred_index)) {
            retval = enif_make_badarg(env);
            goto cleanup;
        }
        pred_index += (tuple_len - 1);
    }

    report = make_report();
    bool result = betree_search_with_event(betree, event, report);

    if(result == false) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    ERL_NIF_TERM res = enif_make_list(env, 0);

	for (size_t i = report->matched; i;) {
		i--;
		res = enif_make_list_cell(env, enif_make_uint64(env, report->subs[i]), res);
	}

    retval = enif_make_tuple2(env, atom_ok, res);
cleanup:
    if(event != NULL) {
        betree_free_event(event);
    }
    if(report != NULL) {
        free_report(report);
    }
    return retval;
}

static ERL_NIF_TERM nif_betree_search_t(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 3) {
        return enif_make_badarg(env);
    }

    int clock_type;
    if (!enif_get_int(env, argv[2], &clock_type)) {
	    return enif_make_badarg(env);
    }
    struct timespec start, done;
    clock_gettime(clock_type, &start);
    ERL_NIF_TERM search_res = nif_betree_search(env, argc - 1, argv);
    if (!enif_is_tuple(env, search_res)) {
        return search_res;
    }
    clock_gettime(clock_type, &done);
    // Convert to microseconds
    ErlNifSInt64 tspent = (done.tv_sec - start.tv_sec) * 1000000 + (done.tv_nsec - start.tv_nsec) / 1000;
    ERL_NIF_TERM etspent = enif_make_int64(env, tspent);
    ERL_NIF_TERM retval = enif_make_tuple2(env, search_res, etspent);
    return retval;
}

static ERL_NIF_TERM nif_betree_exists(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM retval;
    size_t pred_index = 0;
    struct betree_event* event = NULL;

    if(argc != 2) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    struct betree* betree = get_betree(env, argv[0]);
    if(betree == NULL) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    unsigned int list_len;
    if(!enif_get_list_length(env, argv[1], &list_len)) {
        retval = enif_make_badarg(env);
        goto cleanup;
    }

    event = betree_make_event(betree);

    ERL_NIF_TERM head;
    ERL_NIF_TERM tail = argv[1];

    const ERL_NIF_TERM* tuple;
    int tuple_len;

    for(unsigned int i = 0; i < list_len; i++) {
        if(!enif_get_list_cell(env, tail, &head, &tail)) {
            retval = enif_make_badarg(env);
            goto cleanup;
        }

        if(!enif_get_tuple(env, head, &tuple_len, &tuple)) {
            retval = enif_make_badarg(env);
            goto cleanup;
        }

        if(!add_variables(env, betree, event, tuple, tuple_len, pred_index)) {
            retval = enif_make_badarg(env);
            goto cleanup;
        }
        pred_index += (tuple_len - 1);
    }

    bool result = betree_exists_with_event(betree, event);

    ERL_NIF_TERM res = result ? atom_true : atom_false;

    retval = enif_make_tuple2(env, atom_ok, res);
cleanup:
    if(event != NULL) {
        betree_free_event(event);
    }
    return retval;
}

static struct betree_event* make_event(ErlNifEnv* env, struct betree* betree, ERL_NIF_TERM event_tail)
{
    unsigned int list_len;
    if(!enif_get_list_length(env, event_tail, &list_len)) {
        return NULL;
    }

    struct betree_event* event = betree_make_event(betree);
    ERL_NIF_TERM head;

    const ERL_NIF_TERM* tuple;
    int tuple_len;
    size_t pred_index = 0;

    for(unsigned int i = 0; i < list_len; i++) {
        if(!enif_get_list_cell(env, event_tail, &head, &event_tail)) {
            betree_free_event(event);
            return NULL;
        }

        if(!enif_get_tuple(env, head, &tuple_len, &tuple)) {
            betree_free_event(event);
            return NULL;
        }

        if(!add_variables(env, betree, event, tuple, tuple_len, pred_index)) {
            betree_free_event(event);
            return NULL;
        }
        pred_index += (tuple_len - 1);
    }
    return event;
}

static ERL_NIF_TERM betree_search_and_report(ErlNifEnv* env, struct betree* betree, struct betree_event* event)
{
    struct report* report = make_report();
    if (report == NULL) {
        return enif_make_badarg(env);
    }

    bool result = betree_search_with_event(betree, event, report);
    if(result == false) {
        free_report(report);
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM res = enif_make_list(env, 0);
    for (size_t i = report->matched; i;) {
        i--;
        res = enif_make_list_cell(env, enif_make_uint64(env, report->subs[i]), res);
    }
    free_report(report);
    return res;
}

static ERL_NIF_TERM betree_search_continuation(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 1) {
        return enif_make_badarg(env);
    }

    trapped_search_t* trapped_search;
    enif_get_resource(env, argv[0], MEM_TRAPPED_SEARCH, (void**)&trapped_search);
    if (trapped_search == NULL) {
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM res = betree_search_and_report(env, trapped_search->betree, trapped_search->event);
    return enif_make_tuple2(env, atom_ok_continuation, res);
}

static int should_yield(ErlNifEnv* env, int timeslice)
{
    if (timeslice >= 100) return 1;
    if (timeslice <= 0) return 0;
    return enif_consume_timeslice(env, timeslice);
}

static ERL_NIF_TERM schedule_continuation(ErlNifEnv* env, struct betree* betree, struct betree_event*  event)
{
    trapped_search_t* trapped_search =
            enif_alloc_resource(MEM_TRAPPED_SEARCH, sizeof(trapped_search_t));
    if (trapped_search == NULL) {
        betree_free_event(event);
        return enif_make_badarg(env);
    }

    trapped_search->betree = betree;
    trapped_search->event = event;

    ERL_NIF_TERM argv_continuation[1];
    argv_continuation[0] = enif_make_resource(env, trapped_search);
    enif_release_resource(trapped_search);

    return enif_schedule_nif(env,
                             "betree_search_continuation", 0,
                             betree_search_continuation,
                             1, argv_continuation);
}

static ERL_NIF_TERM nif_betree_search_yielding(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 3) {
        return enif_make_badarg(env);
    }

    struct betree* betree = get_betree(env, argv[0]);
    if (betree == NULL) {
        return enif_make_badarg(env);
    }

    struct betree_event* event = make_event(env, betree, argv[1]);
    if (event == NULL) {
        // DO NOT deinit betree, leave it alone!
        // betree_deinit(betree);
        return enif_make_badarg(env);
    }

    bool success;
    int timeslice;
    success = enif_get_int(env, argv[2], &timeslice);
    if (! success) {
        timeslice = 0;
    }

    int must_yield = should_yield(env, timeslice);

    if (must_yield) {
        return schedule_continuation(env, betree, event);
    }

    ERL_NIF_TERM res = betree_search_and_report(env, betree, event);
    return enif_make_tuple2(env, atom_ok, res);
}

/*static ERL_NIF_TERM nif_betree_delete(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])*/
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

static ErlNifFunc nif_functions[] = {
    {"betree_make", 1, nif_betree_make, 0},
    {"betree_make_sub", 4, nif_betree_make_sub, 0},
    {"betree_insert_sub", 2, nif_betree_insert_sub, 0},
    {"betree_exists", 2, nif_betree_exists, 0},
    {"betree_search", 2, nif_betree_search, 0},
    {"betree_search", 3, nif_betree_search_t, 0},
    {"betree_search_yielding", 3, nif_betree_search_yielding, 0}
    /*{"betree_delete", 2, nif_betree_delete, 0}*/
};

ERL_NIF_INIT(erl_betree_nif, nif_functions, &load, NULL, NULL, NULL);

