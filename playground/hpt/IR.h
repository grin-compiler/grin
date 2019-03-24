/*
  abstract program:
  - memory count
  - register count
  - constants ; vector of constant values
  - instruction vector ; contains all instructions
  - block vector ; offset + count into instruction vector
*/
#include <cstdint>
#include <vector>
#include <set>

typedef int32_t simple_type_t;
typedef int32_t tag_t;
typedef int32_t reg_t;
typedef int32_t mem_t;
typedef int32_t constant_id_t; // constant id
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
    constant_id_t constant_id;
    range_t       range;
  };
};

/////////////////////////////////////

// condition
enum condition_type {
  CON_NODE_TYPE_EXISTS  = 200,
  CON_SIMLE_TYPE_EXISTS = 201,
  CON_NOT_IN            = 202,
  CON_ANY               = 203,
  CON_ALL               = 204,
};

struct condition_t {
  enum condition_type type;
  union {
    tag_t         tag;
    simple_type_t simple_type;
    constant_id_t constant_id;
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

// instructions
enum cmd_type {
  CMD_IF                  = 400,
  CMD_PROJECT             = 401,
  CMD_EXTEND              = 402,
  CMD_MOVE                = 403,
  CMD_RESTRICTED_MOVE     = 404,
  CMD_CONDITIONAL_MOVE    = 405,
  CMD_FETCH               = 406,
  CMD_STORE               = 407,
  CMD_UPDATE              = 408,
  CMD_RESTRICTED_UPDATE   = 409,
  CMD_CONDITIONAL_UPDATE  = 410,
  CMD_SET                 = 411,
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
      constant_id_t constant_id;
    } cmd_set;

  };
};

struct abstract_program_t {
  int32_t   memory_count;
  int32_t   register_count;

  std::vector<cmd_t>              cmd;
  std::vector<range_t>            block;
  std::vector<std::set<int32_t>>  constant;
};
/*
struct abstract_program_t {
  int32_t   memory_count;
  int32_t   register_count;
  int32_t   cmd_count;
  int32_t   block_count;

  cmd_t     cmd[];
  range_t   block[];
};
*/
void test (cmd_t c) {
  c.cmd_if.src_reg = 0;
  c.cmd_project.src_reg = 1;
}


// TODO: constant array

/*
  binary format:
  everything is int32

struct range_t {
struct predicate_t {
struct condition_t {
struct selector_t {
struct cmd_t {
*/

//read_range();

abstract_program_t* read_abstract_program(int32_t data[]) {
  return 0;
}
