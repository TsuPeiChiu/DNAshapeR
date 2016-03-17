#ifndef _PROPERTIES_H_
#define _PROPERTIES_H_ 1 

//#include <Rcpp.h>
#include "common.h"
#include "utilities.h"

class properties{
 public:
  properties(){       //simple contructor, initialize all variables
    minor_vector.clear();     //vector that hold minor groove width
    major_vector.clear();
    propel_vector.clear();
    slide1_vector.clear();
    roll1_vector.clear();
    twist1_vector.clear();
    slide2_vector.clear();
    roll2_vector.clear();
    twist2_vector.clear();
    minor_sd=0; minor_ave=0; minor_num=0;
    major_sd=0; major_ave=0; major_num=0;
    propel_sd=0;propel_ave=0; propel_num=0;
    slide1_sd=0; slide1_ave=0; slide1_num=0;
    roll1_sd=0; roll1_ave=0; roll1_num=0;
    twist1_sd=0; twist1_ave=0; twist1_num=0;
    slide2_sd=0; slide2_ave=0; slide2_num=0;
    roll2_sd=0; roll2_ave=0; roll2_num=0;
    twist2_sd=0; twist2_ave=0; twist2_num=0;  
	//Tsu-Pei
	ep_vector.clear();
	ep_sd=0; ep_ave=0; ep_num=0;
  }

  double_vector& get_vector(std::string str);
  
  double get_ave(std::string str);

  int get_num(std::string str);

  double get_sd(std::string str);

  void push(double a,std::string vectorname){
    get_vector(vectorname).push_back(a);
  }

  int get_occurence(std::string vectorname){
    int num_value = get_num(vectorname);
    if (num_value!=0)
      return num_value;
    else{
      double_vector& v=get_vector(vectorname);
      return int(v.size());
    }
  }
  
  void calc_ave_sd(bool);

  void load_data_from_vector(double_vector dv);

 private:
  //slide, roll, twist here are all local inter-base pair parameters
  double_vector minor_vector,major_vector,propel_vector;
  double_vector slide1_vector,roll1_vector,twist1_vector;
  double_vector slide2_vector,roll2_vector,twist2_vector;
  double minor_sd,minor_ave,major_sd,major_ave,propel_ave,propel_sd;
  double slide1_ave,slide1_sd,roll1_ave,roll1_sd,twist1_ave,twist1_sd;
  double slide2_ave,slide2_sd,roll2_ave,roll2_sd,twist2_ave,twist2_sd;
  int minor_num,major_num,propel_num,slide1_num,slide2_num;
  int roll1_num,roll2_num,twist1_num,twist2_num; // size of vector
  void private_vector_ave_sd_calc(double_vector&,double&,double&);
  //Tsu-Pei
  double_vector ep_vector;
  double ep_sd,ep_ave;
  int ep_num;
};

typedef std::vector<properties*>  pointers_vector;


  
#endif
