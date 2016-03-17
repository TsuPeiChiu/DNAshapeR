#include "ReadData.h"
#include "utilities.h"

void read_fasta(std::ifstream& inf,string_vector &sequence_list,string_vector &name_list,bool debug){
  std::string line;
  bool first_line = false;
  bool has_seq_name = false;
  //check if there exist '>'
  while (getline(inf,line)){
    if (line.find(">")!=std::string::npos){
      has_seq_name = true;
      break;
    }
  }

  //reset ifstream
  inf.clear();
  inf.seekg(0, std::ios::beg);

  //has_seq_name
  if (has_seq_name){
    while (getline(inf,line)){
      remove_terminal_space(line);
      if (line[0]=='>'){
	name_list.push_back(line);
	first_line=true;
      }
      else
	if (!line.empty()){
	  convert_to_uppercase(line);
	  if (first_line){
	    sequence_list.push_back(line);
	    first_line=false;
	  }
	  else
	    sequence_list[sequence_list.size()-1]+=line;
	}
    }
  }
  else{
    //does not have seq_name, then one line one record
    int auto_name = 0;
    while (getline(inf,line)){
      remove_terminal_space(line);
      if (!line.empty()){
	auto_name++;
	name_list.push_back(">"+int_to_string(auto_name));
	sequence_list.push_back(line);
      }
    }
  }

  //covert_to_uppercase
  for (unsigned int i=0; i<sequence_list.size(); i++)
    convert_to_uppercase(sequence_list[i]);

  //verify if all sequence is composed of A,T,G,C
  /*
  for (int i=0; i<sequence_list.size(); i++){
    if (sequence_list[i].find_first_not_of("ATGC")!=std::string::npos){
      std::cerr << "Sequence contains unrecognized characters\n"<<sequence_list[i]<<std::endl;
      exit(0);
    }
  }
  */

  if (debug){
    //std::cout<<std::endl;
    for (unsigned int i=0; i<sequence_list.size(); i++){
      //std::cout<<name_list[i]<<std::endl;
      //std::cout<<sequence_list[i]<<std::endl;
    }
  }

  if (name_list.size()!=sequence_list.size()){
    //std::cerr << "Incorrect Fasta Format\n";
    //exit(1);
  }

}



void convert_sequence_list(string_vector& sequence_list,std::vector<pointers_vector> &pointers_matrix,\
			   std::vector<int_vector> &int_matrix,DNA_to_properties& onemap){

  pointers_vector pointers_row;
  int_vector int_row;
  int int_v;      //   0 -- illegal      1  exists in pentamer table      -1  its complementary exists in pentamer table
  properties *pointer_v;
  std::string fragment;
  std::string heptamer;

  pointers_matrix.clear();
  int_matrix.clear();

  for (unsigned int i=0;i<sequence_list.size();i++){
    pointers_row.clear();
    int_row.clear();
    for (unsigned int j=0;j<sequence_list[i].size()-4;j++){
      fragment=sequence_list[i].substr(j,5);
      if (fragment.find_first_not_of("ATGC")!=std::string::npos){
		pointer_v = 0;
		int_v = 0;
      }
      else if (found_str_in_map(fragment,onemap)){
		pointer_v = &onemap[fragment];
		int_v=1;
      }
      else{
		pointer_v = &onemap[opposite_strand(fragment)];
		int_v=-1;
      }
      pointers_row.push_back(pointer_v);
      int_row.push_back(int_v);
    }
    pointer_v = 0;
    int_v = 0;
    for (int j=0;j<4;j++){   // so that the size(pointer_row)==size(sequence)
      pointers_row.push_back(pointer_v);
      int_row.push_back(int_v);
    }


    pointers_matrix.push_back(pointers_row);
    int_matrix.push_back(int_row);
  }
}





