#ifndef _OPTION_PARSER_H_
#define _OPTION_PARSER_H_ 1


#include "common.h"
#include "utilities.h"


class OptionParser{

public:
  OptionParser(){
    name_vector.clear();
    descrip_vector.clear();
    val_vector.clear();    
    req_data_vector.clear();
    number_of_options=0;
  }

  void add_option(std::string name, std::string descrip, \
		  bool req_data, std::string default_val);

  void get_option(std::string name, double &val);

  void get_option(std::string name, int &val);

  void get_option(std::string name, std::string &val);

  void get_option(std::string name, bool &val); // "true" / "false"

  void print_descrip();

  void parse(string_vector &arg);

  int return_index_of_option(std::string name);

  bool is_option(std::string name);



private:

  std::vector<std::string>  name_vector, descrip_vector, val_vector;  
  std::vector<bool> req_data_vector;
  int number_of_options;


};



#endif
