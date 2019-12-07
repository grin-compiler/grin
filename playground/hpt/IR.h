/*
  abstract program:
  - memory count
  - register count
  - constants ; vector of constant values
  - instruction vector ; contains all instructions
  - block vector ; offset + count into instruction vector
*/
#include <boost/container/flat_set.hpp>
#include <cstdint>
#include <vector>
#include <set>
#include <map>
#include <unordered_map>
#include <unordered_set>

typedef int32_t simple_type_t;
typedef int32_t tag_t;
typedef int32_t reg_t;
typedef int32_t mem_t;
typedef int32_t intset_id_t;
typedef int32_t block_id_t;

/////////////////////////////////////

// range
struct range_t {
  int32_t from;
  int32_t to;
};

/////////////////////////////////////

// predicate
enum predicate_type {
  PRE_TAG_IN        = 100,
  PRE_TAG_NOT_IN    = 101,
  PRE_VALUE_IN      = 102,
  PRE_VALUE_NOT_IN  = 103,
};

struct predicate_t {
  enum predicate_type type;
  union {
    intset_id_t   tag_set_id;
    range_t       range;
  };
};

/////////////////////////////////////

// condition
enum condition_type {
  CON_NODE_TYPE_EXISTS    = 200,
  CON_SIMPLE_TYPE_EXISTS  = 201,
  CON_ANY_NOT_IN          = 202,
  CON_ANY                 = 203,
};

struct condition_t {
  enum condition_type type;
  union {
    tag_t         tag;
    simple_type_t simple_type;
    intset_id_t   tag_set_id;
    predicate_t   predicate;
  };
};

/////////////////////////////////////

// selector
enum selector_type {
  SEL_NODE_ITEM             = 300,
  SEL_CONDITION_AS_SELECTOR = 301,
  SEL_ALL_FIELDS            = 302,
};

struct selector_t {
  enum selector_type type;
  union {
    struct {
      tag_t     node_tag;
      int32_t   item_index;
    };
    condition_t condition;
  };
};

/////////////////////////////////////

// constant
enum constant_type {
  CONST_SIMPLE_TYPE   = 400,
  CONST_HEAP_LOCATION = 401,
  CONST_NODE_TYPE     = 402,
  CONST_NODE_ITEM     = 403,
};

struct constant_t {
  enum constant_type type;
  union {
    simple_type_t simple_type;
    mem_t         mem;
    struct {
      tag_t     node_tag;
      int32_t   item_index; // index or arity
      int32_t   item_value;
    };
  };
};

/////////////////////////////////////

// instructions
enum cmd_type {
  CMD_IF                  = 500,
  CMD_PROJECT             = 501,
  CMD_EXTEND              = 502,
  CMD_MOVE                = 503,
  CMD_RESTRICTED_MOVE     = 504,
  CMD_CONDITIONAL_MOVE    = 505,
  CMD_FETCH               = 506,
  CMD_STORE               = 507,
  CMD_UPDATE              = 508,
  CMD_RESTRICTED_UPDATE   = 509,
  CMD_CONDITIONAL_UPDATE  = 510,
  CMD_SET                 = 511,
};

struct cmd_t {

  enum cmd_type type;

  union {

    struct {
      condition_t   condition;
      reg_t         src_reg;
      block_id_t    block_id;
    } cmd_if;

    struct {
      selector_t    src_selector;
      reg_t         src_reg;
      reg_t         dst_reg;
    } cmd_project;

    struct {
      reg_t         src_reg;
      selector_t    dst_selector;
      reg_t         dst_reg;
    } cmd_extend;

    struct {
      reg_t         src_reg;
      reg_t         dst_reg;
    } cmd_move;

    struct {
      reg_t         src_reg;
      reg_t         dst_reg;
    } cmd_restricted_move;

    struct {
      reg_t         src_reg;
      predicate_t   predicate;
      reg_t         dst_reg;
    } cmd_conditional_move;

    struct {
      reg_t         address_reg;
      reg_t         dst_reg;
    } cmd_fetch;

    struct {
      reg_t         src_reg;
      mem_t         address;
    } cmd_store;

    struct {
      reg_t         src_reg;
      reg_t         address_reg;
    } cmd_update;

    struct {
      reg_t         src_reg;
      reg_t         address_reg;
    } cmd_restricted_update;

    struct {
      reg_t         src_reg;
      predicate_t   predicate;
      reg_t         address_reg;
    } cmd_conditional_update;

    struct {
      reg_t         dst_reg;
      constant_t    constant;
    } cmd_set;

  };
};

struct abstract_program_t {
  int32_t     memory_count;
  int32_t     register_count;
  block_id_t  start_block_id;

  std::vector<cmd_t>              cmd;
  std::vector<range_t>            block;
  std::vector<std::set<int32_t>>  intset;
};

// abstract values for evaluation and result serialization

enum result_type {
  RES_INT_SET   = 1000,
  RES_NODE_ITEM = 1001,
  RES_NODE_SET  = 1002,
  RES_VALUE     = 1003,
};

typedef boost::container::flat_set<int32_t> int_set_t;
typedef std::unordered_map<int32_t, std::vector<int_set_t>> node_set_t;

// NOTE: GRIN is a typed language, a register can have only one type from the following options: simple_type, location, node
struct value_t {
  int_set_t   simple_type;
  node_set_t  node_set;
};

abstract_program_t *load_abstract_program(char *name);
void eval_abstract_program(char *name);

void save_result_file(const char* name, int32_t iter_count, std::vector<node_set_t>& mem, std::vector<value_t>& reg);
