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

	//add ep
	ep_vector.clear();
	ep_sd=0; ep_ave=0; ep_num=0;

	//add mc
	MGD_mc_vector.clear();
	Stretch_vector.clear();
	Tilt1_vector.clear();
	Tilt2_vector.clear();
	Buckle_vector.clear();
	MGW_mc_vector.clear();
	Roll_mc1_vector.clear();
	Roll_mc2_vector.clear();
	Shear_vector.clear();
	Opening_vector.clear();
	Rise1_vector.clear();
	Rise2_vector.clear();
	Shift1_vector.clear();
	Shift2_vector.clear();
	Stagger_vector.clear();
	ProT_mc_vector.clear();
	mGD_mc_vector.clear();
	Slide1_vector.clear();
	Slide2_vector.clear();
	HelT_mc1_vector.clear();
	HelT_mc2_vector.clear();
	mGW_mc_vector.clear();
	MGD_mc_ave=0;MGD_mc_sd=0;MGD_mc_num=0;
    Stretch_ave=0;Stretch_sd=0;Stretch_num=0;
    Tilt1_ave=0;Tilt1_sd=0;Tilt1_num=0;
    Tilt2_ave=0;Tilt2_sd=0;Tilt2_num=0;
    Buckle_ave=0;Buckle_sd=0;Buckle_num=0;
    MGW_mc_ave=0;MGW_mc_sd=0;MGW_mc_num=0;
    Roll_mc1_ave=0;Roll_mc1_sd=0;Roll_mc1_num=0;
    Roll_mc2_ave=0;Roll_mc2_sd=0;Roll_mc2_num=0;
    Shear_ave=0;Shear_sd=0;Shear_num=0;
    Opening_ave=0;Opening_sd=0;Opening_num=0;
    Rise1_ave=0;Rise1_sd=0;Rise1_num=0;
    Rise2_ave=0;Rise2_sd=0;Rise2_num=0;
    Shift1_ave=0;Shift1_sd=0;Shift1_num=0;
    Shift2_ave=0;Shift2_sd=0;Shift2_num=0;
    Stagger_ave=0;Stagger_sd=0;Stagger_num=0;
    ProT_mc_ave=0;ProT_mc_sd=0;ProT_mc_num=0;
    mGD_mc_ave=0;mGD_mc_sd=0;mGD_mc_num=0;
    Slide1_ave=0;Slide1_sd=0;Slide1_num=0;
    Slide2_ave=0;Slide2_sd=0;Slide2_num=0;
    HelT_mc1_ave=0;HelT_mc1_sd=0;HelT_mc1_num=0;
    HelT_mc2_ave=0;HelT_mc2_sd=0;HelT_mc2_num=0;
    mGW_mc_ave=0;mGW_mc_sd=0;mGW_mc_num=0;

	//add xrc
	ProT_xrc_vector.clear();
	Tilt_xrc1_vector.clear();
	Tilt_xrc2_vector.clear();
	Buckle_xrc_vector.clear();
	Roll_xrc1_vector.clear();
	Roll_xrc2_vector.clear();
	Shear_xrc_vector.clear();
	Opening_xrc_vector.clear();
	Rise_xrc1_vector.clear();
	Rise_xrc2_vector.clear();
	Stretch_xrc_vector.clear();
	HelT_xrc1_vector.clear();
	HelT_xrc2_vector.clear();
	Shift_xrc1_vector.clear();
	Shift_xrc2_vector.clear();
	Slide_xrc1_vector.clear();
	Slide_xrc2_vector.clear();
	Stagger_xrc_vector.clear();
	MGW_xrc_vector.clear();
    ProT_xrc_ave=0;ProT_xrc_sd=0;ProT_xrc_num=0;
    Tilt_xrc1_ave=0;Tilt_xrc1_sd=0;Tilt_xrc1_num=0;
    Tilt_xrc2_ave=0;Tilt_xrc2_sd=0;Tilt_xrc2_num=0;
    Buckle_xrc_ave=0;Buckle_xrc_sd=0;Buckle_xrc_num=0;
    Roll_xrc1_ave=0;Roll_xrc1_sd=0;Roll_xrc1_num=0;
    Roll_xrc2_ave=0;Roll_xrc2_sd=0;Roll_xrc2_num=0;
    Shear_xrc_ave=0;Shear_xrc_sd=0;Shear_xrc_num=0;
    Opening_xrc_ave=0;Opening_xrc_sd=0;Opening_xrc_num=0;
    Rise_xrc1_ave=0;Rise_xrc1_sd=0;Rise_xrc1_num=0;
    Rise_xrc2_ave=0;Rise_xrc2_sd=0;Rise_xrc2_num=0;
    Stretch_xrc_ave=0;Stretch_xrc_sd=0;Stretch_xrc_num=0;
    HelT_xrc1_ave=0;HelT_xrc1_sd=0;HelT_xrc1_num=0;
    HelT_xrc2_ave=0;HelT_xrc2_sd=0;HelT_xrc2_num=0;
    Shift_xrc1_ave=0;Shift_xrc1_sd=0;Shift_xrc1_num=0;
    Shift_xrc2_ave=0;Shift_xrc2_sd=0;Shift_xrc2_num=0;
    Slide_xrc1_ave=0;Slide_xrc1_sd=0;Slide_xrc1_num=0;
    Slide_xrc2_ave=0;Slide_xrc2_sd=0;Slide_xrc2_num=0;
    Stagger_xrc_ave=0;Stagger_xrc_sd=0;Stagger_xrc_num=0;
    MGW_xrc_ave=0;MGW_xrc_sd=0;MGW_xrc_num=0;

	//add md
	ProT_md_vector.clear();
	Tilt_md1_vector.clear();
	Tilt_md2_vector.clear();
	Buckle_md_vector.clear();
	Roll_md1_vector.clear();
	Roll_md2_vector.clear();
	Shear_md_vector.clear();
	Opening_md_vector.clear();
	Rise_md1_vector.clear();
	Rise_md2_vector.clear();
	Stretch_md_vector.clear();
	HelT_md1_vector.clear();
	HelT_md2_vector.clear();
	Shift_md1_vector.clear();
	Shift_md2_vector.clear();
	Slide_md1_vector.clear();
	Slide_md2_vector.clear();
	Stagger_md_vector.clear();
	MGW_md_vector.clear();
    ProT_md_ave=0;ProT_md_sd=0;ProT_md_num=0;
    Tilt_md1_ave=0;Tilt_md1_sd=0;Tilt_md1_num=0;
    Tilt_md2_ave=0;Tilt_md2_sd=0;Tilt_md2_num=0;
    Buckle_md_ave=0;Buckle_md_sd=0;Buckle_md_num=0;
    Roll_md1_ave=0;Roll_md1_sd=0;Roll_md1_num=0;
    Roll_md2_ave=0;Roll_md2_sd=0;Roll_md2_num=0;
    Shear_md_ave=0;Shear_md_sd=0;Shear_md_num=0;
    Opening_md_ave=0;Opening_md_sd=0;Opening_md_num=0;
    Rise_md1_ave=0;Rise_md1_sd=0;Rise_md1_num=0;
    Rise_md2_ave=0;Rise_md2_sd=0;Rise_md2_num=0;
    Stretch_md_ave=0;Stretch_md_sd=0;Stretch_md_num=0;
    HelT_md1_ave=0;HelT_md1_sd=0;HelT_md1_num=0;
    HelT_md2_ave=0;HelT_md2_sd=0;HelT_md2_num=0;
    Shift_md1_ave=0;Shift_md1_sd=0;Shift_md1_num=0;
    Shift_md2_ave=0;Shift_md2_sd=0;Shift_md2_num=0;
    Slide_md1_ave=0;Slide_md1_sd=0;Slide_md1_num=0;
    Slide_md2_ave=0;Slide_md2_sd=0;Slide_md2_num=0;
    Stagger_md_ave=0;Stagger_md_sd=0;Stagger_md_num=0;
    MGW_md_ave=0;MGW_md_sd=0;MGW_md_num=0;
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

  //add ep
  double_vector ep_vector;
  double ep_sd,ep_ave;
  int ep_num;

  //add mc
  double_vector MGD_mc_vector,Stretch_vector,Tilt1_vector,
	Tilt2_vector,Buckle_vector,MGW_mc_vector,Roll_mc1_vector,
	Roll_mc2_vector,Shear_vector,Opening_vector,Rise1_vector,
	Rise2_vector,Shift1_vector,Shift2_vector,Stagger_vector,
	ProT_mc_vector,mGD_mc_vector,Slide1_vector,Slide2_vector,
	HelT_mc1_vector,HelT_mc2_vector,mGW_mc_vector;
  double MGD_mc_ave,MGD_mc_sd,Stretch_ave,Stretch_sd,
    Tilt1_ave,Tilt1_sd,Tilt2_ave,Tilt2_sd,
    Buckle_ave,Buckle_sd,MGW_mc_ave,MGW_mc_sd,
    Roll_mc1_ave,Roll_mc1_sd,Roll_mc2_ave,Roll_mc2_sd,
    Shear_ave,Shear_sd,Opening_ave,Opening_sd,
    Rise1_ave,Rise1_sd,Rise2_ave,Rise2_sd,
    Shift1_ave,Shift1_sd,Shift2_ave,Shift2_sd,
    Stagger_ave,Stagger_sd,ProT_mc_ave,ProT_mc_sd,
    mGD_mc_ave,mGD_mc_sd,Slide1_ave,Slide1_sd,
    Slide2_ave,Slide2_sd,HelT_mc1_ave,HelT_mc1_sd,
    HelT_mc2_ave,HelT_mc2_sd,mGW_mc_ave,mGW_mc_sd;
  int MGD_mc_num,Stretch_num,Tilt1_num,Tilt2_num,Buckle_num,MGW_mc_num,
    Roll_mc1_num,Roll_mc2_num,Shear_num,Opening_num,Rise1_num,
	Rise2_num,Shift1_num,Shift2_num,Stagger_num,ProT_mc_num,
	mGD_mc_num,Slide1_num,Slide2_num,HelT_mc1_num,HelT_mc2_num,mGW_mc_num;

  //add xrc
  double_vector ProT_xrc_vector,Tilt_xrc1_vector,Tilt_xrc2_vector,Buckle_xrc_vector,
    Roll_xrc1_vector,Roll_xrc2_vector,Shear_xrc_vector,Opening_xrc_vector,
	Rise_xrc1_vector,Rise_xrc2_vector,Stretch_xrc_vector,HelT_xrc1_vector,
	HelT_xrc2_vector,Shift_xrc1_vector,Shift_xrc2_vector,Slide_xrc1_vector,
	Slide_xrc2_vector,Stagger_xrc_vector,MGW_xrc_vector;
  double ProT_xrc_ave,ProT_xrc_sd,Tilt_xrc1_ave,Tilt_xrc1_sd,Tilt_xrc2_ave,Tilt_xrc2_sd,
    Buckle_xrc_ave,Buckle_xrc_sd,Roll_xrc1_ave,Roll_xrc1_sd,Roll_xrc2_ave,Roll_xrc2_sd,
    Shear_xrc_ave,Shear_xrc_sd,Opening_xrc_ave,Opening_xrc_sd,Rise_xrc1_ave,Rise_xrc1_sd,
    Rise_xrc2_ave,Rise_xrc2_sd,Stretch_xrc_ave,Stretch_xrc_sd,HelT_xrc1_ave,HelT_xrc1_sd,
    HelT_xrc2_ave,HelT_xrc2_sd,Shift_xrc1_ave,Shift_xrc1_sd,Shift_xrc2_ave,Shift_xrc2_sd,
    Slide_xrc1_ave,Slide_xrc1_sd,Slide_xrc2_ave,Slide_xrc2_sd,Stagger_xrc_ave,Stagger_xrc_sd,
    MGW_xrc_ave,MGW_xrc_sd;
  int ProT_xrc_num,Tilt_xrc1_num,Tilt_xrc2_num,Buckle_xrc_num,Roll_xrc1_num,Roll_xrc2_num,
    Shear_xrc_num,Opening_xrc_num,Rise_xrc1_num,Rise_xrc2_num,Stretch_xrc_num,HelT_xrc1_num,
    HelT_xrc2_num,Shift_xrc1_num,Shift_xrc2_num,Slide_xrc1_num,Slide_xrc2_num,Stagger_xrc_num,
    MGW_xrc_num;

  //add md
  double_vector ProT_md_vector,Tilt_md1_vector,Tilt_md2_vector,Buckle_md_vector,
    Roll_md1_vector,Roll_md2_vector,Shear_md_vector,Opening_md_vector,
	Rise_md1_vector,Rise_md2_vector,Stretch_md_vector,HelT_md1_vector,
	HelT_md2_vector,Shift_md1_vector,Shift_md2_vector,Slide_md1_vector,
	Slide_md2_vector,Stagger_md_vector,MGW_md_vector;
  double ProT_md_ave,ProT_md_sd,Tilt_md1_ave,Tilt_md1_sd,Tilt_md2_ave,Tilt_md2_sd,
    Buckle_md_ave,Buckle_md_sd,Roll_md1_ave,Roll_md1_sd,Roll_md2_ave,Roll_md2_sd,
    Shear_md_ave,Shear_md_sd,Opening_md_ave,Opening_md_sd,Rise_md1_ave,Rise_md1_sd,
    Rise_md2_ave,Rise_md2_sd,Stretch_md_ave,Stretch_md_sd,HelT_md1_ave,HelT_md1_sd,
    HelT_md2_ave,HelT_md2_sd,Shift_md1_ave,Shift_md1_sd,Shift_md2_ave,Shift_md2_sd,
    Slide_md1_ave,Slide_md1_sd,Slide_md2_ave,Slide_md2_sd,Stagger_md_ave,Stagger_md_sd,
    MGW_md_ave,MGW_md_sd;
  int ProT_md_num,Tilt_md1_num,Tilt_md2_num,Buckle_md_num,Roll_md1_num,Roll_md2_num,
    Shear_md_num,Opening_md_num,Rise_md1_num,Rise_md2_num,Stretch_md_num,HelT_md1_num,
    HelT_md2_num,Shift_md1_num,Shift_md2_num,Slide_md1_num,Slide_md2_num,Stagger_md_num,
    MGW_md_num;
};

typedef std::vector<properties*>  pointers_vector;



#endif
