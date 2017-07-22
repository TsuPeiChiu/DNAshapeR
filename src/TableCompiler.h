#ifndef _TABLECOMPILER_H_ 
#define _TABLECOMPILER_H_ 1


#include <Rcpp.h>
#include "common.h"
#include "utilities.h"
#include "map.h"
#include "properties.h"

void add_groove_width_to_pentamers_table(std::string filename,DNA_to_properties& onemap,std::string sequence,int object_index, \
					 std::string object_name,bool verbose,bool debug,bool five);

void add_ep_to_pentamers_table(std::string filename, DNA_to_properties& onemap, std::string sequence, int object_index, \
					 std::string object_name, bool verbose, bool debug, bool five);
					 
void add_groove_width_to_inosine_table(std::string filename,DNA_to_properties& onemap,std::string sequence,int object_index, \
					 std::string object_name,bool verbose,bool debug,bool five);


void add_step_info_to_pentamers_table(DNA_to_properties& onemap,std::string filename,std::string sequence,bool verbose,bool debug);

void add_one_step_info(DNA_to_properties& onemap,std::string sequence, std::vector<double_vector> &matrix,int object_index,\
		       std::string object_name,bool verbose,bool debug);

void add_propel_to_table(DNA_to_properties& onemap,std::string sequence, std::vector<double_vector> &matrix,int object_index,\
			 std::string object_name,bool verbose,bool debug);

void process_querytable_file(std::string,DNA_to_properties&,bool debug);

void process_querytable(DNA_to_properties&,bool debug);

#endif
