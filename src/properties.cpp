#include "properties.h"

void properties::private_vector_ave_sd_calc(double_vector &v,double &ave,double &sd){
	for (unsigned int i=0;i<v.size();i++)
		ave+=v[i];
	ave=ave/v.size();

	if (v.size()>1){
		for (unsigned int i=0; i<v.size(); i++)
			sd+=pow(v[i]-ave,2);
		sd=sqrt(sd/(v.size()-1));
	}else
		sd=0;
}

double_vector& properties::get_vector(std::string str){
  if (str=="minor")
    return minor_vector;
  if (str=="major")
    return major_vector;
  if (str=="propel")
    return propel_vector;
  if (str=="slide1")
    return slide1_vector;
  if (str=="roll1")
    return roll1_vector;
  if (str=="twist1")
    return twist1_vector;
  if (str=="slide2")
    return slide2_vector;
  if (str=="roll2")
    return roll2_vector;
  if (str=="twist2")
    return twist2_vector;

  //add ep
  if (str=="ep")
	  return ep_vector;

  //add mc
  if (str=="MGD_mc")
	  return MGD_mc_vector;
  if (str=="Stretch")
	  return Stretch_vector;
  if (str=="Tilt1")
	  return Tilt1_vector;
  if (str=="Tilt2")
	  return Tilt2_vector;
  if (str=="Buckle")
	  return Buckle_vector;
  if (str=="MGW_mc")
	  return MGW_mc_vector;
  if (str=="Roll_mc1")
	  return Roll_mc1_vector;
  if (str=="Roll_mc2")
	  return Roll_mc2_vector;
  if (str=="Shear")
	  return Shear_vector;
  if (str=="Opening")
	  return Opening_vector;
  if (str=="Rise1")
	  return Rise1_vector;
  if (str=="Rise2")
	  return Rise2_vector;
  if (str=="Shift1")
	  return Shift1_vector;
  if (str=="Shift2")
	  return Shift2_vector;
  if (str=="Stagger")
	  return Stagger_vector;
  if (str=="ProT_mc")
	  return ProT_mc_vector;
  if (str=="mGD_mc")
	  return mGD_mc_vector;
  if (str=="Slide1")
	  return Slide1_vector;
  if (str=="Slide2")
	  return Slide2_vector;
  if (str=="HelT_mc1")
	  return HelT_mc1_vector;
  if (str=="HelT_mc2")
	  return HelT_mc2_vector;
  if (str=="mGW_mc")
	  return mGW_mc_vector;

  //add xrc
  if (str=="ProT_xrc")
	  return ProT_xrc_vector;
  if (str=="Tilt_xrc1")
	  return Tilt_xrc1_vector;
  if (str=="Tilt_xrc2")
	  return Tilt_xrc2_vector;
  if (str=="Buckle_xrc")
	  return Buckle_xrc_vector;
  if (str=="Roll_xrc1")
	  return Roll_xrc1_vector;
  if (str=="Roll_xrc2")
	  return Roll_xrc2_vector;
  if (str=="Shear_xrc")
	  return Shear_xrc_vector;
  if (str=="Opening_xrc")
	  return Opening_xrc_vector;
  if (str=="Rise_xrc1")
	  return Rise_xrc1_vector;
  if (str=="Rise_xrc2")
	  return Rise_xrc2_vector;
  if (str=="Stretch_xrc")
	  return Stretch_xrc_vector;
  if (str=="HelT_xrc1")
	  return HelT_xrc1_vector;
  if (str=="HelT_xrc2")
	  return HelT_xrc2_vector;
  if (str=="Shift_xrc1")
	  return Shift_xrc1_vector;
  if (str=="Shift_xrc2")
	  return Shift_xrc2_vector;
  if (str=="Slide_xrc1")
	  return Slide_xrc1_vector;
  if (str=="Slide_xrc2")
	  return Slide_xrc2_vector;
  if (str=="Stagger_xrc")
	  return Stagger_xrc_vector;
  if (str=="MGW_xrc")
	  return MGW_xrc_vector;

  //add md
  if (str=="ProT_md")
	  return ProT_md_vector;
  if (str=="Tilt_md1")
	  return Tilt_md1_vector;
  if (str=="Tilt_md2")
	  return Tilt_md2_vector;
  if (str=="Buckle_md")
	  return Buckle_md_vector;
  if (str=="Roll_md1")
	  return Roll_md1_vector;
  if (str=="Roll_md2")
	  return Roll_md2_vector;
  if (str=="Shear_md")
	  return Shear_md_vector;
  if (str=="Opening_md")
	  return Opening_md_vector;
  if (str=="Rise_md1")
	  return Rise_md1_vector;
  if (str=="Rise_md2")
	  return Rise_md2_vector;
  if (str=="Stretch_md")
	  return Stretch_md_vector;
  if (str=="HelT_md1")
	  return HelT_md1_vector;
  if (str=="HelT_md2")
	  return HelT_md2_vector;
  if (str=="Shift_md1")
	  return Shift_md1_vector;
  if (str=="Shift_md2")
	  return Shift_md2_vector;
  if (str=="Slide_md1")
	  return Slide_md1_vector;
  if (str=="Slide_md2")
	  return Slide_md2_vector;
  if (str=="Stagger_md")
	  return Stagger_md_vector;
  if (str=="MGW_md")
	  return MGW_md_vector;

  else
    return minor_vector;
}

int properties::get_num(std::string str){
  if (str=="minor")
    return minor_num;
  if (str=="major")
    return major_num;
  if (str=="propel")
    return propel_num;
  if (str=="slide1")
    return slide1_num;
  if (str=="roll1")
    return roll1_num;
  if (str=="twist1")
    return twist1_num;
  if (str=="slide2")
    return slide2_num;
  if (str=="roll2")
    return roll2_num;
  if (str=="twist2")
    return twist2_num;

  //add ep
  if (str=="ep")
    return ep_num;

  //add mc
  if (str=="MGD_mc")
    return MGD_mc_num;
  if (str=="Stretch")
    return Stretch_num;
  if (str=="Tilt1")
    return Tilt1_num;
  if (str=="Tilt2")
    return Tilt2_num;
  if (str=="Buckle")
    return Buckle_num;
  if (str=="MGW_mc")
    return MGW_mc_num;
  if (str=="Roll_mc1")
    return Roll_mc1_num;
  if (str=="Roll_mc2")
    return Roll_mc2_num;
  if (str=="Shear")
    return Shear_num;
  if (str=="Opening")
    return Opening_num;
  if (str=="Rise1")
    return Rise1_num;
  if (str=="Rise2")
    return Rise2_num;
  if (str=="Shift1")
    return Shift1_num;
  if (str=="Shift2")
    return Shift2_num;
  if (str=="Stagger")
    return Stagger_num;
  if (str=="ProT_mc")
    return ProT_mc_num;
  if (str=="mGD_mc")
    return mGD_mc_num;
  if (str=="Slide1")
    return Slide1_num;
  if (str=="Slide2")
    return Slide2_num;
  if (str=="HelT_mc1")
    return HelT_mc1_num;
  if (str=="HelT_mc2")
    return HelT_mc2_num;
  if (str=="mGW_mc")
    return mGW_mc_num;

  //add xrc
  if (str=="ProT_xrc")
    return ProT_xrc_num;
  if (str=="Tilt_xrc1")
    return Tilt_xrc1_num;
  if (str=="Tilt_xrc2")
    return Tilt_xrc2_num;
  if (str=="Buckle_xrc")
    return Buckle_xrc_num;
  if (str=="Roll_xrc1")
    return Roll_xrc1_num;
  if (str=="Roll_xrc2")
    return Roll_xrc2_num;
  if (str=="Shear_xrc")
    return Shear_xrc_num;
  if (str=="Opening_xrc")
    return Opening_xrc_num;
  if (str=="Rise_xrc1")
    return Rise_xrc1_num;
  if (str=="Rise_xrc2")
    return Rise_xrc2_num;
  if (str=="Stretch_xrc")
    return Stretch_xrc_num;
  if (str=="HelT_xrc1")
    return HelT_xrc1_num;
  if (str=="HelT_xrc2")
    return HelT_xrc2_num;
  if (str=="Shift_xrc1")
    return Shift_xrc1_num;
  if (str=="Shift_xrc2")
    return Shift_xrc2_num;
  if (str=="Slide_xrc1")
    return Slide_xrc1_num;
  if (str=="Slide_xrc2")
    return Slide_xrc2_num;
  if (str=="Stagger_xrc")
    return Stagger_xrc_num;
  if (str=="MGW_xrc")
    return MGW_xrc_num;

  //add md
  if (str=="ProT_md")
    return ProT_md_num;
  if (str=="Tilt_md1")
    return Tilt_md1_num;
  if (str=="Tilt_md2")
    return Tilt_md2_num;
  if (str=="Buckle_md")
    return Buckle_md_num;
  if (str=="Roll_md1")
    return Roll_md1_num;
  if (str=="Roll_md2")
    return Roll_md2_num;
  if (str=="Shear_md")
    return Shear_md_num;
  if (str=="Opening_md")
    return Opening_md_num;
  if (str=="Rise_md1")
    return Rise_md1_num;
  if (str=="Rise_md2")
    return Rise_md2_num;
  if (str=="Stretch_md")
    return Stretch_md_num;
  if (str=="HelT_md1")
    return HelT_md1_num;
  if (str=="HelT_md2")
    return HelT_md2_num;
  if (str=="Shift_md1")
    return Shift_md1_num;
  if (str=="Shift_md2")
    return Shift_md2_num;
  if (str=="Slide_md1")
    return Slide_md1_num;
  if (str=="Slide_md2")
    return Slide_md2_num;
  if (str=="Stagger_md")
    return Stagger_md_num;
  if (str=="MGW_md")
    return MGW_md_num;

  else
    return minor_num;
}


double properties::get_ave(std::string str){
  if (str=="minor")
    return minor_ave;
  if (str=="major")
    return major_ave;
  if (str=="propel")
    return propel_ave;
  if (str=="slide1")
    return slide1_ave;
  if (str=="roll1")
    return roll1_ave;
  if (str=="twist1")
    return twist1_ave;
  if (str=="slide2")
    return slide2_ave;
  if (str=="roll2")
    return roll2_ave;
  if (str=="twist2")
    return twist2_ave;

  //add ep
  if (str=="ep")
    return ep_ave;

  //add mc
  if (str=="MGD_mc")
    return MGD_mc_ave;
  if (str=="Stretch")
    return Stretch_ave;
  if (str=="Tilt1")
    return Tilt1_ave;
  if (str=="Tilt2")
    return Tilt2_ave;
  if (str=="Buckle")
    return Buckle_ave;
  if (str=="MGW_mc")
    return MGW_mc_ave;
  if (str=="Roll_mc1")
    return Roll_mc1_ave;
  if (str=="Roll_mc2")
    return Roll_mc2_ave;
  if (str=="Shear")
    return Shear_ave;
  if (str=="Opening")
    return Opening_ave;
  if (str=="Rise1")
    return Rise1_ave;
  if (str=="Rise2")
    return Rise2_ave;
  if (str=="Shift1")
    return Shift1_ave;
  if (str=="Shift2")
    return Shift2_ave;
  if (str=="Stagger")
    return Stagger_ave;
  if (str=="ProT_mc")
    return ProT_mc_ave;
  if (str=="mGD_mc")
    return mGD_mc_ave;
  if (str=="Slide1")
    return Slide1_ave;
  if (str=="Slide2")
    return Slide2_ave;
  if (str=="HelT_mc1")
    return HelT_mc1_ave;
  if (str=="HelT_mc2")
    return HelT_mc2_ave;
  if (str=="mGW_mc")
    return mGW_mc_ave;

  //add xrc
  if (str=="ProT_xrc")
    return ProT_xrc_ave;
  if (str=="Tilt_xrc1")
    return Tilt_xrc1_ave;
  if (str=="Tilt_xrc2")
    return Tilt_xrc2_ave;
  if (str=="Buckle_xrc")
    return Buckle_xrc_ave;
  if (str=="Roll_xrc1")
    return Roll_xrc1_ave;
  if (str=="Roll_xrc2")
    return Roll_xrc2_ave;
  if (str=="Shear_xrc")
    return Shear_xrc_ave;
  if (str=="Opening_xrc")
    return Opening_xrc_ave;
  if (str=="Rise_xrc1")
    return Rise_xrc1_ave;
  if (str=="Rise_xrc2")
    return Rise_xrc2_ave;
  if (str=="Stretch_xrc")
    return Stretch_xrc_ave;
  if (str=="HelT_xrc1")
    return HelT_xrc1_ave;
  if (str=="HelT_xrc2")
    return HelT_xrc2_ave;
  if (str=="Shift_xrc1")
    return Shift_xrc1_ave;
  if (str=="Shift_xrc2")
    return Shift_xrc2_ave;
  if (str=="Slide_xrc1")
    return Slide_xrc1_ave;
  if (str=="Slide_xrc2")
    return Slide_xrc2_ave;
  if (str=="Stagger_xrc")
    return Stagger_xrc_ave;
  if (str=="MGW_xrc")
    return MGW_xrc_ave;

  //add md
  if (str=="ProT_md")
    return ProT_md_ave;
  if (str=="Tilt_md1")
    return Tilt_md1_ave;
  if (str=="Tilt_md2")
    return Tilt_md2_ave;
  if (str=="Buckle_md")
    return Buckle_md_ave;
  if (str=="Roll_md1")
    return Roll_md1_ave;
  if (str=="Roll_md2")
    return Roll_md2_ave;
  if (str=="Shear_md")
    return Shear_md_ave;
  if (str=="Opening_md")
    return Opening_md_ave;
  if (str=="Rise_md1")
    return Rise_md1_ave;
  if (str=="Rise_md2")
    return Rise_md2_ave;
  if (str=="Stretch_md")
    return Stretch_md_ave;
  if (str=="HelT_md1")
    return HelT_md1_ave;
  if (str=="HelT_md2")
    return HelT_md2_ave;
  if (str=="Shift_md1")
    return Shift_md1_ave;
  if (str=="Shift_md2")
    return Shift_md2_ave;
  if (str=="Slide_md1")
    return Slide_md1_ave;
  if (str=="Slide_md2")
    return Slide_md2_ave;
  if (str=="Stagger_md")
    return Stagger_md_ave;
  if (str=="MGW_md")
    return MGW_md_ave;

  else
    return minor_ave;
}

double properties::get_sd(std::string str){
  if (str=="minor")
    return minor_sd;
  if (str=="major")
    return major_sd;
  if (str=="propel")
    return propel_sd;
  if (str=="slide1")
    return slide1_sd;
  if (str=="roll1")
    return roll1_sd;
  if (str=="twist1")
    return twist1_sd;
  if (str=="slide2")
    return slide2_sd;
  if (str=="roll2")
    return roll2_sd;
  if (str=="twist2")
    return twist2_sd;

  //add ep
  if (str=="ep")
    return ep_sd;

  //add mc
  if (str=="MGD_mc")
    return MGD_mc_sd;
  if (str=="Stretch")
    return Stretch_sd;
  if (str=="Tilt1")
    return Tilt1_sd;
  if (str=="Tilt2")
    return Tilt2_sd;
  if (str=="Buckle")
    return Buckle_sd;
  if (str=="MGW_mc")
    return MGW_mc_sd;
  if (str=="Roll_mc1")
    return Roll_mc1_sd;
  if (str=="Roll_mc2")
    return Roll_mc2_sd;
  if (str=="Shear")
    return Shear_sd;
  if (str=="Opening")
    return Opening_sd;
  if (str=="Rise1")
    return Rise1_sd;
  if (str=="Rise2")
    return Rise2_sd;
  if (str=="Shift1")
    return Shift1_sd;
  if (str=="Shift2")
    return Shift2_sd;
  if (str=="Stagger")
    return Stagger_sd;
  if (str=="ProT_mc")
    return ProT_mc_sd;
  if (str=="mGD_mc")
    return mGD_mc_sd;
  if (str=="Slide1")
    return Slide1_sd;
  if (str=="Slide2")
    return Slide2_sd;
  if (str=="HelT_mc1")
    return HelT_mc1_sd;
  if (str=="HelT_mc2")
    return HelT_mc2_sd;
  if (str=="mGW_mc")
    return mGW_mc_sd;

  //add xrc
  if (str=="ProT_xrc")
    return ProT_xrc_sd;
  if (str=="Tilt_xrc1")
    return Tilt_xrc1_sd;
  if (str=="Tilt_xrc2")
    return Tilt_xrc2_sd;
  if (str=="Buckle_xrc")
    return Buckle_xrc_sd;
  if (str=="Roll_xrc1")
    return Roll_xrc1_sd;
  if (str=="Roll_xrc2")
    return Roll_xrc2_sd;
  if (str=="Shear_xrc")
    return Shear_xrc_sd;
  if (str=="Opening_xrc")
    return Opening_xrc_sd;
  if (str=="Rise_xrc1")
    return Rise_xrc1_sd;
  if (str=="Rise_xrc2")
    return Rise_xrc2_sd;
  if (str=="Stretch_xrc")
    return Stretch_xrc_sd;
  if (str=="HelT_xrc1")
    return HelT_xrc1_sd;
  if (str=="HelT_xrc2")
    return HelT_xrc2_sd;
  if (str=="Shift_xrc1")
    return Shift_xrc1_sd;
  if (str=="Shift_xrc2")
    return Shift_xrc2_sd;
  if (str=="Slide_xrc1")
    return Slide_xrc1_sd;
  if (str=="Slide_xrc2")
    return Slide_xrc2_sd;
  if (str=="Stagger_xrc")
    return Stagger_xrc_sd;
  if (str=="MGW_xrc")
    return MGW_xrc_sd;

  //add md
  if (str=="ProT_md")
    return ProT_md_sd;
  if (str=="Tilt_md1")
    return Tilt_md1_sd;
  if (str=="Tilt_md2")
    return Tilt_md2_sd;
  if (str=="Buckle_md")
    return Buckle_md_sd;
  if (str=="Roll_md1")
    return Roll_md1_sd;
  if (str=="Roll_md2")
    return Roll_md2_sd;
  if (str=="Shear_md")
    return Shear_md_sd;
  if (str=="Opening_md")
    return Opening_md_sd;
  if (str=="Rise_md1")
    return Rise_md1_sd;
  if (str=="Rise_md2")
    return Rise_md2_sd;
  if (str=="Stretch_md")
    return Stretch_md_sd;
  if (str=="HelT_md1")
    return HelT_md1_sd;
  if (str=="HelT_md2")
    return HelT_md2_sd;
  if (str=="Shift_md1")
    return Shift_md1_sd;
  if (str=="Shift_md2")
    return Shift_md2_sd;
  if (str=="Slide_md1")
    return Slide_md1_sd;
  if (str=="Slide_md2")
    return Slide_md2_sd;
  if (str=="Stagger_md")
    return Stagger_md_sd;
  if (str=="MGW_md")
    return MGW_md_sd;

  else
    return minor_sd;
}

void properties::calc_ave_sd(bool debug){
  //calculate the average
  /*
  if (debug){
    std::cout << "Inside calc_ave_sd()" << std::endl;
    std::cout << "Size of Minor Vector: " <<minor_vector.size() << std::endl;
    std::cout << "Size of Major Vector: " <<major_vector.size() << std::endl;
    std::cout << "Size of Slide1 Vector: " <<slide1_vector.size() << std::endl;
    std::cout << "Size of Roll1 Vector : " <<roll1_vector.size() << std::endl;
    std::cout << "Size of Twist1 Vector: " <<twist1_vector.size() << std::endl;
    std::cout << "Size of Slide2 Vector: " <<slide2_vector.size() << std::endl;
    std::cout << "Size of Roll2 Vector : " <<roll2_vector.size() << std::endl;
    std::cout << "Size of Twist2 Vector: " <<twist2_vector.size() << std::endl;
  }
  */

  private_vector_ave_sd_calc(minor_vector,minor_ave,minor_sd);
  private_vector_ave_sd_calc(major_vector,major_ave,major_sd);
  private_vector_ave_sd_calc(propel_vector,propel_ave,propel_sd);
  private_vector_ave_sd_calc(slide1_vector,slide1_ave,slide1_sd);
  private_vector_ave_sd_calc(roll1_vector,roll1_ave,roll1_sd);
  private_vector_ave_sd_calc(twist1_vector,twist1_ave,twist1_sd);
  private_vector_ave_sd_calc(slide2_vector,slide2_ave,slide2_sd);
  private_vector_ave_sd_calc(roll2_vector,roll2_ave,roll2_sd);
  private_vector_ave_sd_calc(twist2_vector,twist2_ave,twist2_sd);

  //add ep
  private_vector_ave_sd_calc(ep_vector,ep_ave,ep_sd);

  //add mc
  private_vector_ave_sd_calc(MGD_mc_vector,MGD_mc_ave,MGD_mc_sd);
  private_vector_ave_sd_calc(Stretch_vector,Stretch_ave,Stretch_sd);
  private_vector_ave_sd_calc(Tilt1_vector,Tilt1_ave,Tilt1_sd);
  private_vector_ave_sd_calc(Tilt2_vector,Tilt2_ave,Tilt2_sd);
  private_vector_ave_sd_calc(Buckle_vector,Buckle_ave,Buckle_sd);
  private_vector_ave_sd_calc(MGW_mc_vector,MGW_mc_ave,MGW_mc_sd);
  private_vector_ave_sd_calc(Roll_mc1_vector,Roll_mc1_ave,Roll_mc1_sd);
  private_vector_ave_sd_calc(Roll_mc2_vector,Roll_mc2_ave,Roll_mc2_sd);
  private_vector_ave_sd_calc(Shear_vector,Shear_ave,Shear_sd);
  private_vector_ave_sd_calc(Opening_vector,Opening_ave,Opening_sd);
  private_vector_ave_sd_calc(Rise1_vector,Rise1_ave,Rise1_sd);
  private_vector_ave_sd_calc(Rise2_vector,Rise2_ave,Rise2_sd);
  private_vector_ave_sd_calc(Shift1_vector,Shift1_ave,Shift1_sd);
  private_vector_ave_sd_calc(Shift2_vector,Shift2_ave,Shift2_sd);
  private_vector_ave_sd_calc(Stagger_vector,Stagger_ave,Stagger_sd);
  private_vector_ave_sd_calc(ProT_mc_vector,ProT_mc_ave,ProT_mc_sd);
  private_vector_ave_sd_calc(mGD_mc_vector,mGD_mc_ave,mGD_mc_sd);
  private_vector_ave_sd_calc(Slide1_vector,Slide1_ave,Slide1_sd);
  private_vector_ave_sd_calc(Slide2_vector,Slide2_ave,Slide2_sd);
  private_vector_ave_sd_calc(HelT_mc1_vector,HelT_mc1_ave,HelT_mc1_sd);
  private_vector_ave_sd_calc(HelT_mc2_vector,HelT_mc2_ave,HelT_mc2_sd);
  private_vector_ave_sd_calc(mGW_mc_vector,mGW_mc_ave,mGW_mc_sd);

  //add xrc
  private_vector_ave_sd_calc(ProT_xrc_vector,ProT_xrc_ave,ProT_xrc_sd);
  private_vector_ave_sd_calc(Tilt_xrc1_vector,Tilt_xrc1_ave,Tilt_xrc1_sd);
  private_vector_ave_sd_calc(Tilt_xrc2_vector,Tilt_xrc2_ave,Tilt_xrc2_sd);
  private_vector_ave_sd_calc(Buckle_xrc_vector,Buckle_xrc_ave,Buckle_xrc_sd);
  private_vector_ave_sd_calc(Roll_xrc1_vector,Roll_xrc1_ave,Roll_xrc1_sd);
  private_vector_ave_sd_calc(Roll_xrc2_vector,Roll_xrc2_ave,Roll_xrc2_sd);
  private_vector_ave_sd_calc(Shear_xrc_vector,Shear_xrc_ave,Shear_xrc_sd);
  private_vector_ave_sd_calc(Opening_xrc_vector,Opening_xrc_ave,Opening_xrc_sd);
  private_vector_ave_sd_calc(Rise_xrc1_vector,Rise_xrc1_ave,Rise_xrc1_sd);
  private_vector_ave_sd_calc(Rise_xrc2_vector,Rise_xrc2_ave,Rise_xrc2_sd);
  private_vector_ave_sd_calc(Stretch_xrc_vector,Stretch_xrc_ave,Stretch_xrc_sd);
  private_vector_ave_sd_calc(HelT_xrc1_vector,HelT_xrc1_ave,HelT_xrc1_sd);
  private_vector_ave_sd_calc(HelT_xrc2_vector,HelT_xrc2_ave,HelT_xrc2_sd);
  private_vector_ave_sd_calc(Shift_xrc1_vector,Shift_xrc1_ave,Shift_xrc1_sd);
  private_vector_ave_sd_calc(Shift_xrc2_vector,Shift_xrc2_ave,Shift_xrc2_sd);
  private_vector_ave_sd_calc(Slide_xrc1_vector,Slide_xrc1_ave,Slide_xrc1_sd);
  private_vector_ave_sd_calc(Slide_xrc2_vector,Slide_xrc2_ave,Slide_xrc2_sd);
  private_vector_ave_sd_calc(Stagger_xrc_vector,Stagger_xrc_ave,Stagger_xrc_sd);
  private_vector_ave_sd_calc(MGW_xrc_vector,MGW_xrc_ave,MGW_xrc_sd);

  //add md
  private_vector_ave_sd_calc(ProT_md_vector,ProT_md_ave,ProT_md_sd);
  private_vector_ave_sd_calc(Tilt_md1_vector,Tilt_md1_ave,Tilt_md1_sd);
  private_vector_ave_sd_calc(Tilt_md2_vector,Tilt_md2_ave,Tilt_md2_sd);
  private_vector_ave_sd_calc(Buckle_md_vector,Buckle_md_ave,Buckle_md_sd);
  private_vector_ave_sd_calc(Roll_md1_vector,Roll_md1_ave,Roll_md1_sd);
  private_vector_ave_sd_calc(Roll_md2_vector,Roll_md2_ave,Roll_md2_sd);
  private_vector_ave_sd_calc(Shear_md_vector,Shear_md_ave,Shear_md_sd);
  private_vector_ave_sd_calc(Opening_md_vector,Opening_md_ave,Opening_md_sd);
  private_vector_ave_sd_calc(Rise_md1_vector,Rise_md1_ave,Rise_md1_sd);
  private_vector_ave_sd_calc(Rise_md2_vector,Rise_md2_ave,Rise_md2_sd);
  private_vector_ave_sd_calc(Stretch_md_vector,Stretch_md_ave,Stretch_md_sd);
  private_vector_ave_sd_calc(HelT_md1_vector,HelT_md1_ave,HelT_md1_sd);
  private_vector_ave_sd_calc(HelT_md2_vector,HelT_md2_ave,HelT_md2_sd);
  private_vector_ave_sd_calc(Shift_md1_vector,Shift_md1_ave,Shift_md1_sd);
  private_vector_ave_sd_calc(Shift_md2_vector,Shift_md2_ave,Shift_md2_sd);
  private_vector_ave_sd_calc(Slide_md1_vector,Slide_md1_ave,Slide_md1_sd);
  private_vector_ave_sd_calc(Slide_md2_vector,Slide_md2_ave,Slide_md2_sd);
  private_vector_ave_sd_calc(Stagger_md_vector,Stagger_md_ave,Stagger_md_sd);
  private_vector_ave_sd_calc(MGW_md_vector,MGW_md_ave,MGW_md_sd);
}

void properties::load_data_from_vector(double_vector dv){
  minor_ave = dv[0];  minor_sd = dv[1]; minor_num = int(dv[2]);
  major_ave = dv[3];  major_sd = dv[4]; major_num = int(dv[5]);
  propel_ave = dv[6]; propel_sd = dv[7]; propel_num = int(dv[8]);
  slide1_ave = dv[9]; slide1_sd = dv[10]; slide1_num = int(dv[11]);
  slide2_ave = dv[12]; slide2_sd = dv[13]; slide2_num = int(dv[14]);
  roll1_ave = dv[15]; roll1_sd = dv[16]; roll1_num = int(dv[17]);
  roll2_ave = dv[18]; roll2_sd = dv[19]; roll2_num = int(dv[20]);
  twist1_ave = dv[21]; twist1_sd = dv[22]; twist1_num = int(dv[23]);
  twist2_ave = dv[24]; twist2_sd = dv[25]; twist2_num = int(dv[26]);

  //add ep
  ep_ave = dv[27]; ep_sd = dv[28]; ep_num = dv[29];

  //add mc
  MGD_mc_ave = dv[30]; MGD_mc_sd = 0; MGD_mc_num = 0;
  Stretch_ave = dv[31]; Stretch_sd = 0; Stretch_num = 0;
  Tilt1_ave = dv[32]; Tilt1_sd = 0; Tilt1_num = 0;
  Tilt2_ave = dv[33]; Tilt2_sd = 0; Tilt2_num = 0;
  Buckle_ave = dv[34]; Buckle_sd = 0; Buckle_num = 0;
  MGW_mc_ave = dv[35]; MGW_mc_sd = 0; MGW_mc_num = 0;
  Roll_mc1_ave = dv[36]; Roll_mc1_sd = 0; Roll_mc1_num = 0;
  Roll_mc2_ave = dv[37]; Roll_mc2_sd = 0; Roll_mc2_num = 0;
  Shear_ave = dv[38]; Shear_sd = 0; Shear_num = 0;
  Opening_ave = dv[39]; Opening_sd = 0; Opening_num = 0;
  Rise1_ave = dv[40]; Rise1_sd = 0; Rise1_num = 0;
  Rise2_ave = dv[41]; Rise2_sd = 0; Rise2_num = 0;
  Shift1_ave = dv[42]; Shift1_sd = 0; Shift1_num = 0;
  Shift2_ave = dv[43]; Shift2_sd = 0; Shift2_num = 0;
  Stagger_ave = dv[44]; Stagger_sd = 0; Stagger_num = 0;
  ProT_mc_ave = dv[45]; ProT_mc_sd = 0; ProT_mc_num = 0;
  mGD_mc_ave = dv[46]; mGD_mc_sd = 0; mGD_mc_num = 0;
  Slide1_ave = dv[47]; Slide1_sd = 0; Slide1_num = 0;
  Slide2_ave = dv[48]; Slide2_sd = 0; Slide2_num = 0;
  HelT_mc1_ave = dv[49]; HelT_mc1_sd = 0; HelT_mc1_num = 0;
  HelT_mc2_ave = dv[50]; HelT_mc2_sd = 0; HelT_mc2_num = 0;
  mGW_mc_ave = dv[51]; mGW_mc_sd = 0; mGW_mc_num = 0;

  //add xrc
  ProT_xrc_ave = dv[52]; ProT_xrc_sd = 0; ProT_xrc_num = 0;
  Tilt_xrc1_ave = dv[53]; Tilt_xrc1_sd = 0; Tilt_xrc1_num = 0;
  Tilt_xrc2_ave = dv[54]; Tilt_xrc2_sd = 0; Tilt_xrc2_num = 0;
  Buckle_xrc_ave = dv[55]; Buckle_xrc_sd = 0; Buckle_xrc_num = 0;
  Roll_xrc1_ave = dv[56]; Roll_xrc1_sd = 0; Roll_xrc1_num = 0;
  Roll_xrc2_ave = dv[57]; Roll_xrc2_sd = 0; Roll_xrc2_num = 0;
  Shear_xrc_ave = dv[58]; Shear_xrc_sd = 0; Shear_xrc_num = 0;
  Opening_xrc_ave = dv[59]; Opening_xrc_sd = 0; Opening_xrc_num = 0;
  Rise_xrc1_ave = dv[60]; Rise_xrc1_sd = 0; Rise_xrc1_num = 0;
  Rise_xrc2_ave = dv[61]; Rise_xrc2_sd = 0; Rise_xrc2_num = 0;
  Stretch_xrc_ave = dv[62]; Stretch_xrc_sd = 0; Stretch_xrc_num = 0;
  HelT_xrc1_ave = dv[63]; HelT_xrc1_sd = 0; HelT_xrc1_num = 0;
  HelT_xrc2_ave = dv[64]; HelT_xrc2_sd = 0; HelT_xrc2_num = 0;
  Shift_xrc1_ave = dv[65]; Shift_xrc1_sd = 0; Shift_xrc1_num = 0;
  Shift_xrc2_ave = dv[66]; Shift_xrc2_sd = 0; Shift_xrc2_num = 0;
  Slide_xrc1_ave = dv[67]; Slide_xrc1_sd = 0; Slide_xrc1_num = 0;
  Slide_xrc2_ave = dv[68]; Slide_xrc2_sd = 0; Slide_xrc2_num = 0;
  Stagger_xrc_ave = dv[69]; Stagger_xrc_sd = 0; Stagger_xrc_num = 0;
  MGW_xrc_ave = dv[70]; MGW_xrc_sd = 0; MGW_xrc_num = 0;

  //add md
  ProT_md_ave = dv[71]; ProT_md_sd = 0; ProT_md_num = 0;
  Tilt_md1_ave = dv[72]; Tilt_md1_sd = 0; Tilt_md1_num = 0;
  Tilt_md2_ave = dv[73]; Tilt_md2_sd = 0; Tilt_md2_num = 0;
  Buckle_md_ave = dv[74]; Buckle_md_sd = 0; Buckle_md_num = 0;
  Roll_md1_ave = dv[75]; Roll_md1_sd = 0; Roll_md1_num = 0;
  Roll_md2_ave = dv[76]; Roll_md2_sd = 0; Roll_md2_num = 0;
  Shear_md_ave = dv[77]; Shear_md_sd = 0; Shear_md_num = 0;
  Opening_md_ave = dv[78]; Opening_md_sd = 0; Opening_md_num = 0;
  Rise_md1_ave = dv[79]; Rise_md1_sd = 0; Rise_md1_num = 0;
  Rise_md2_ave = dv[82]; Rise_md2_sd = 0; Rise_md2_num = 0;
  Stretch_md_ave = dv[81]; Stretch_md_sd = 0; Stretch_md_num = 0;
  HelT_md1_ave = dv[82]; HelT_md1_sd = 0; HelT_md1_num = 0;
  HelT_md2_ave = dv[83]; HelT_md2_sd = 0; HelT_md2_num = 0;
  Shift_md1_ave = dv[84]; Shift_md1_sd = 0; Shift_md1_num = 0;
  Shift_md2_ave = dv[85]; Shift_md2_sd = 0; Shift_md2_num = 0;
  Slide_md1_ave = dv[86]; Slide_md1_sd = 0; Slide_md1_num = 0;
  Slide_md2_ave = dv[87]; Slide_md2_sd = 0; Slide_md2_num = 0;
  Stagger_md_ave = dv[88]; Stagger_md_sd = 0; Stagger_md_num = 0;
  MGW_md_ave = dv[89]; MGW_md_sd = 0; MGW_md_num = 0;

}

