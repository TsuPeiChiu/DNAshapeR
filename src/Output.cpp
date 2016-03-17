#include "Output.h"
#include "map.h"

void output_pentamers_map(DNA_to_properties &pentamers_map,string_vector &object_list,bool inosine){
  DNA_to_properties::const_iterator end = pentamers_map.end();
  for (DNA_to_properties::iterator it = pentamers_map.begin(); it != end; ++it){
    //std::cout<<std::setw(7)<<it->first;
    if (!inosine)
      //std::cout <<std::setw(7)<<opposite_strand(it->first);
    //std::cout << std::fixed;
    for (unsigned int i = 0; i < object_list.size(); i++){
      //std::cout << std::setw(7)<< std::setprecision(2)<<it->second.get_ave(object_list[i]);
      //std::cout << std::setw(7)<< std::setprecision(2)<<it->second.get_sd(object_list[i]);
      //std::cout << std::setw(7)<< std::setprecision(2)<<it->second.get_occurence(object_list[i]);
    }
    //std::cout << std::endl;
  }
}

void output_pentamers_map_to_querytable(DNA_to_properties &pentamers_map,std::string querytable_filename){
  DNA_to_properties::const_iterator end = pentamers_map.end();
  std::ofstream qt_ofstream(querytable_filename.c_str());
  string_vector output_list;
  output_list.clear();
  output_list.push_back("minor");
  output_list.push_back("major");
  output_list.push_back("propel");
  output_list.push_back("slide1");
  output_list.push_back("slide2");
  output_list.push_back("roll1");
  output_list.push_back("roll2");
  output_list.push_back("twist1");
  output_list.push_back("twist2");
  output_list.push_back("ep"); //by Tsu-Pei

  for (DNA_to_properties::iterator it = pentamers_map.begin(); it!= end; ++it){
    qt_ofstream << std::setw(7) << it->first;
    qt_ofstream << std::fixed;
    for (unsigned int i=0; i< output_list.size(); i++){
      qt_ofstream << std::setw(8) << std::setprecision(2) << it->second.get_ave(output_list[i]);
      qt_ofstream << std::setw(6) << std::setprecision(2) << it->second.get_sd(output_list[i]);
      qt_ofstream << std::setw(5) << std::setprecision(1) << it->second.get_occurence(output_list[i]);
    }
    qt_ofstream << std::endl;
  }
  qt_ofstream.close();
}

void step_parameters_distribution(DNA_to_properties &pentamers_map,std::string parameter_name){
  // for all 10 unique dinucleotides, print all data points (standard deviation vs. average )
  // AA, AT, AG, AC, TA, TG, TC, GG, GC, CG

  typedef std::map<std::string,int> Dimer_to_index;
  Dimer_to_index dimer_map;
  std::string alphabet[4] = {"A","T","G","C"};
  std::string dimer;
  int current_index=-1;
  dimer_map.clear();

  for (int i=0;i<4;i++)
    for (int j=0;j<4;j++){
      dimer = alphabet[i] + alphabet[j];
      Dimer_to_index::const_iterator dimer_end = dimer_map.end();
      if ((dimer_map.find(dimer)==dimer_end) and (dimer_map.find(opposite_strand(dimer))==dimer_end)){
	current_index++;
	dimer_map[dimer]=current_index;
      }
    }

  std::vector<double_vector> value_matrix;
  std::vector<double_vector> sd_matrix;
  value_matrix.clear();
  sd_matrix.clear();

  double_vector empty_double_vector;
  empty_double_vector.clear();
  for (int i=0;i<=current_index;i++){
    value_matrix.push_back(empty_double_vector);
    sd_matrix.push_back(empty_double_vector);
  }

  Dimer_to_index::const_iterator dimer_end = dimer_map.end();
  DNA_to_properties::const_iterator pentamer_end = pentamers_map.end();

  std::string p1 = parameter_name + "1";
  std::string p2 = parameter_name + "2";

  for (DNA_to_properties::iterator it = pentamers_map.begin(); it != pentamer_end; ++it){
    std::string pentamer = it->first;
    std::string first_dimer = pentamer.substr(1,2);
    std::string second_dimer = pentamer.substr(2,2);

    if (dimer_map.find(first_dimer)!=dimer_end){
      value_matrix[dimer_map[first_dimer]].push_back(it->second.get_ave(p1));
      sd_matrix[dimer_map[first_dimer]].push_back(it->second.get_sd(p1));
    }
    else{
      value_matrix[dimer_map[opposite_strand(first_dimer)]].push_back(it->second.get_ave(p1));
      sd_matrix[dimer_map[opposite_strand(first_dimer)]].push_back(it->second.get_sd(p1));
    }

    if (dimer_map.find(second_dimer)!=dimer_end){
      value_matrix[dimer_map[second_dimer]].push_back(it->second.get_ave(p2));
      sd_matrix[dimer_map[second_dimer]].push_back(it->second.get_sd(p2));
    }
    else{
      value_matrix[dimer_map[opposite_strand(second_dimer)]].push_back(it->second.get_ave(p2));
      sd_matrix[dimer_map[opposite_strand(second_dimer)]].push_back(it->second.get_sd(p2));
    }
  }

  //for (Dimer_to_index::iterator it = dimer_map.begin(); it!=dimer_end; ++it){
    //std::cout<<std::endl<<it->first<<"\t"<<value_matrix[it->second].size()<<std::endl;
    //std::cout << std::fixed;
  //  for (unsigned int i=0; i<value_matrix[it->second].size(); i++){
      //std::cout << std::setw(7)<< std::setprecision(2)<<value_matrix[it->second][i];
      //std::cout << std::setw(7)<< std::setprecision(2)<<sd_matrix[it->second][i];
      //std::cout << std::endl;
  //  }
  //}

}


void output_stringstream_to_file(std::stringstream &ss,std::string filename){
  std::ofstream outf(filename.c_str());
  outf << ss.rdbuf();
  outf.close();
}
