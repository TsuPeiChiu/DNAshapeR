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
  //Tsu-Pei
  if (str=="ep")
	  return ep_vector;
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
  //Tsu-Pei
  if (str=="ep")
    return ep_num;
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
  //Tsu-Pei
  if (str=="ep")
    return ep_ave;
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
  //Tsu-Pei
  if (str=="ep")
    return ep_sd;
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
  //Tsu-Pei
  private_vector_ave_sd_calc(ep_vector,ep_ave,ep_sd);
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
  //Tsu-Pei
  ep_ave = dv[27]; ep_sd = dv[28]; ep_num = int(dv[29]);
}
