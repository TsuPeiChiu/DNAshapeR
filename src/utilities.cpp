#include "utilities.h"

using std::string;

void convert_to_uppercase(std::string &str){
  string::iterator i=str.begin();
  string::iterator end=str.end();
  while (i!=end){
    *i=std::toupper((unsigned char)*i);
    ++i;
  }
}

void parse_string_to_list(std::string str,std::string token,string_vector &list){

  list.clear();
  int pos_of_last_token=-1;
  int status=2;    //  1--token" ,;"     2--non-token
  for (unsigned int i=0;i<str.length();i++){

    if (token.find_first_of(str[i])!=string::npos){   //current pos is token
      if (status==2){        //previous one is non-token
	status=1;
	if ((i-pos_of_last_token-1)>0){     //the item is not empty
	  string item=str.substr(pos_of_last_token+1,i-pos_of_last_token-1);
	  list.push_back(item);
	}
      }
      pos_of_last_token=i;
    }
    else{
      status=2;
      if (i==str.length()-1){
	string item=str.substr(pos_of_last_token+1,i-pos_of_last_token);
	list.push_back(item);
      }
    }
  }
  //process the last item of the string
}

std::string opposite_strand(std::string str){

  string tt;
  tt.clear();
  for (int i=str.size()-1;i>=0;i--)
    tt+=get_opposite_base(str[i]);
  return tt;
}

std::string get_exec_path(){
  //char buf[PATH_MAX+1];
  //if (readlink("/proc/self/exe",buf, sizeof(buf)-1)==-1){
  //  std::cerr << "readlin() failed" <<std::endl;
  //  exit(1);
  //}
  //std::string str(buf);
  //return str.substr(0,str.rfind('/'));
  return 0;
}

char get_opposite_base(char ch){
  switch (ch){
  case 'A':
    return 'T';
  case 'T':
    return 'A';
  case 'G':
    return 'C';
  case 'C':
    return 'G';
  default:
    //std::cerr<<"Failed to find opposite base"<<std::endl;
    //exit(1);
    return '-';
  }
}

double string_to_double(std::string str){
  double num;
  std::istringstream buffer(str);
  buffer>>num;
  return num;
}

int string_to_int(std::string str){
  int num;
  std::istringstream buffer(str);
  buffer>>num;
  return num;
}

std::string int_to_string(int num){
  std::stringstream buffer;
  buffer << num;
  return buffer.str();
}

void remove_terminal_space(std::string &str){
  while ((!str.empty()) and ((str[0]==' ') or (str[0]=='\r')))
    str=str.substr(1,str.length());
  while ((!str.empty()) and ((str[str.length()-1]==' ') or (str[str.length()-1]=='\r')))
    str=str.substr(0,str.length()-1);
}

bool is_DNA(std::string str){
  for (unsigned int i=0;i<str.size();i++){
    if (str.substr(i,1).find_first_of("ATGC")==std::string::npos)
      return false;
  }
  return true;
}

bool is_NUM(std::string str){
  for (unsigned int i=0; i<str.size();i++){
      if (str.substr(i,1).find_first_of("0123456789.")==std::string::npos)
	return false;
    }
  return true;
}

bool string_vector_to_double_vector(string_vector str_v,double_vector &double_v){
  double_v.clear();
  for (unsigned int i=0;i<str_v.size();i++){
    double double_num;
    std::istringstream buffer(str_v[i]);
    if (!(buffer>>double_num))
      return false;
    double_v.push_back(double_num);
  }
  return true;
}

int double_to_int(double d){
  int num=(int)d;
  return num;
}

int get_time(){
  time_t seconds;
  seconds = time(NULL);
  return (int)seconds;
}
