#ifndef _MAP_H_
#define _MAP_H_ 1

#include "common.h"
#include "properties.h"

typedef std::map<std::string,properties> DNA_to_properties;

void build_unique_pentamers(DNA_to_properties&);
  
bool found_str_in_map(std::string,DNA_to_properties&);



#endif
