#include "OptionParser.h"

void OptionParser::add_option(std::string name, std::string descrip, \
			      bool req_data, std::string default_val){
  number_of_options++;
  name_vector.push_back(name);
  descrip_vector.push_back(descrip);
  req_data_vector.push_back(req_data);
  val_vector.push_back(default_val);
}


int OptionParser::return_index_of_option(std::string name){
  for(int i=0;i<number_of_options;i++)
    if (name_vector[i]==name)
      return i;

  //std::cerr<< "Cannot find the Option \n";
  //exit(1);
  return -1;
}

bool OptionParser::is_option(std::string name){
  for (int i=0;i<number_of_options;i++)
    if (name_vector[i]==name)
      return true;
  return false;
}


void OptionParser::get_option(std::string name, double &val){
  std::string value=val_vector[return_index_of_option(name)];
  val=string_to_double(value);
}

void OptionParser::get_option(std::string name, int &val){
  std::string value=val_vector[return_index_of_option(name)];
  val=string_to_int(value);
}

void OptionParser::get_option(std::string name, std::string &val){
  std::string value=val_vector[return_index_of_option(name)];
  val=value;
}

void OptionParser::get_option(std::string name, bool &val){
  std::string value=val_vector[return_index_of_option(name)];
  if (value=="true")
    val=true;
  else if (value=="false")
    val=false;
  else {
    //std::cerr<<"Bool Option got an invalid value\n";
    //exit(1);
  }
}

void OptionParser::print_descrip(){
  //using std::cout;
  //cout <<"\n";
  //cout<<"Name \n";
  //cout <<"\t High-throughput Genome-wide DNA shape prediction\n";
  //cout <<"\t Author:  Tianyin Zhou ( zhoutianyin@hotmail.com)\n";
  //cout << "\n";
  //cout <<"SYNOPSIS \n";
  //cout <<"\n";
  //cout <<"\t[program] [OPTIONS] ";
  //cout<<"\n";
  //cout<<"OPTIONS\n";
  //cout<<"\n";
  //for (int i=0; i<number_of_options; i++){
  //  if (req_data_vector[i])
  //    cout <<"\t"<<name_vector[i]<<" [value]\n";
  //  else
  //    cout <<"\t"<<name_vector[i]<<"\n";
  //  cout << "\t"<<descrip_vector[i]<<"\n";
  //  cout << "\n";
  //}
  //cout <<"\n";
  //exit(0);
}


void OptionParser::parse(string_vector &arg){
  int waiting_option_index=-1;
  bool option_waiting=false;
  for (unsigned int i=0; i<arg.size(); i++){
    if ((arg[i][0]=='-') and (is_option(arg[i]))){

	if (option_waiting){
	  //std::cerr<<"Cannot parse the argument 1 "<<arg[i]<<"\n";
	  //exit(1);
	}

	int index = return_index_of_option(arg[i]);
	if (!req_data_vector[index])
	  val_vector[index]="true";
	else{
	  option_waiting=true;
	  waiting_option_index=index;
	}

      }
      else {
	if (!option_waiting){
	  //std::cerr<<"Cannot parse the argument 2 "<<arg[i]<<"\n";
	  //exit(1);
	}

	val_vector[waiting_option_index]=arg[i];
	option_waiting=false;
      }
  }
  if (option_waiting){
    //std::cerr<<"Cannot parse the end argment\n";
    //exit(1);
  }

  //test code
  //std::cout<<"Size of vector: "<<val_vector.size()<<std::endl;
  //for (int i=0; i<val_vector.size();i++)
  //  std::cout<<i<<"th: "<<val_vector[i]<<std::endl;



}









