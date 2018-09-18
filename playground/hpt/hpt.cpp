#include <vector>
#include <unordered_map>
#include <unordered_set>

/*
  node_item_t = set<simple_type_t> | set<location>
  node_data_t = vector<node_item_t>

  heap:       loc -> node_value_t
  registers:  reg -> value_t

  node_value_t  = unordered_map<tag_t, node_data_t>
  value_t       = set<simple_type_t> | node_value_t | set<location>
*/
using namespace std;

typedef unordered_set<int32_t> node_item_t ; // 0 or positive = location ; negative = simple type
typedef vector<node_item_t> node_data_t;

typedef uint32_t tag_t;
typedef unordered_map<tag_t, node_data_t> node_set_t;

// NOTE: GRIN is a typed language, a register can have only one type from the following options: simple_type, location, node
class value_t {
public:
  unordered_set<int32_t>    *simple_type_and_location_set;
  node_set_t                *node_set;
};

vector<node_set_t> heap_location;
vector<value_t> grin_register;

/*
  operations:
    - register value type check
    - register += register  ; value_t += value_t
    - register += location  ; value_t += node_set_t
    - location += register  ; node_set_t += value_t (only the node set part)
    - register += register (only nodes, i.e. node bind)
    - register += simple type constant
    - register += tag constant
    - register += location constant
    - register += node constant
*/

/*
  case dispatch - valid for the following types
    node (tag value)
    tag value
    simple type value

  bind
    full value - always ok
    node deconstruct - ok if
      - have only node values
      - pattern arity not smaller then the biggest node
*/
