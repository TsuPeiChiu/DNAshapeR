#ifndef _UTILITIES_H_
#define _UTILITIES_H_ 1


#include "common.h"
#include <time.h>
#include <unistd.h>
#include <limits.h>
#include <sstream>
#include <fstream>

void convert_to_uppercase(std::string&);

void parse_string_to_list(std::string,std::string,string_vector&);

double string_to_double(std::string);

int string_to_int(std::string);

std::string int_to_string(int);

std::string opposite_strand(std::string);

char get_opposite_base(char);

std::string get_exec_path();

void remove_terminal_space(std::string&);

bool is_DNA(std::string);

bool is_NUM(std::string);

bool string_vector_to_double_vector(string_vector,double_vector&);

int double_to_int(double);

int get_time();

#endif
