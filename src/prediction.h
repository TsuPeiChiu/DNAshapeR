#include "common.h"
#include "map.h"

void predict_groove_width(std::stringstream&,std::vector<pointers_vector>&,std::vector<int_vector>&,string_vector&,bool,\
			  DNA_to_properties&,std::string,int,char);

void predict_step_parameters(std::stringstream&,std::vector<pointers_vector>&,std::vector<int_vector>&,string_vector&,bool,\
			     DNA_to_properties&,std::string,int,char);

void print_newline_or_delimiter(std::stringstream&,int,int,char);

void predict_groove_width_inosine(std::string,string_vector&,string_vector&,DNA_to_properties&,DNA_to_properties&);
