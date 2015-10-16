#include "common.h"
#include "map.h"


void read_fasta(std::ifstream&,string_vector&,string_vector&,bool);

//convert_sequence_list to a matrix of pointers(class properties)
void convert_sequence_list(string_vector&,std::vector<pointers_vector>&,std::vector<int_vector>&,DNA_to_properties&);

