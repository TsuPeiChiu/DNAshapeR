#include "prediction.h"
#include "utilities.h"

//Can be used to predict propel as well
void predict_groove_width(std::stringstream &outs,std::vector<pointers_vector> &pointers_matrix,std::vector<int_vector> &status_matrix, \
			  string_vector &name_list,bool debug,DNA_to_properties &pentamers_map,std::string objectname,int width,char delimiter){
  int f_index;   //the starting position of the pentamer

  for (unsigned int i=0;i<name_list.size();i++){
	int no_of_printed = 2;
    outs<<name_list[i]<<std::endl;
    outs<<"NA"<<delimiter<<"NA"<<delimiter;
    outs<< std::fixed;
    for (unsigned int j=2;j<pointers_matrix[i].size()-1;j++){
		no_of_printed++;
		if (j==pointers_matrix[i].size()-2){
			outs<<"NA";
			print_newline_or_delimiter(outs,no_of_printed,width,delimiter);
			continue;
		}
		f_index = j-2;
		if (status_matrix[i][f_index]==0)
			outs<<"NA";
		else{
			//Modified by Tsu-Pei
			if (status_matrix[i][f_index]==1){
				outs<<std::setprecision(2)<< (*pointers_matrix[i][f_index]).get_ave(objectname);
			}
			else
				outs<<std::setprecision(2)<< (*pointers_matrix[i][f_index]).get_ave(objectname);
		}
		print_newline_or_delimiter(outs,no_of_printed,width,delimiter);
    }
    outs<<"NA"<<std::endl;
  }
}


void predict_step_parameters(std::stringstream &outs,std::vector<pointers_vector> &pointers_matrix,std::vector<int_vector> &status_matrix,string_vector &namelist, \
			     bool debug,DNA_to_properties &pentamers_map,std::string objectname,int width,char delimiter){
  //if (debug)
  //  std::cout << "Predicting step parameter: "<<objectname << std::endl;

  int f1_index,f2_index;
  std::string object1,object2;
  double value1,value2;
  object1 = objectname+"1";
  object2 = objectname+"2";

  for (unsigned int i=0;i<namelist.size();i++){
    int no_of_printed = 0;
    outs<<namelist[i]<<std::endl;
    outs<< std::fixed;
    //if len(seq)=N, then total number of steps is (N-1)
    //the first and the last step cannot be predicted
    for (unsigned int j=0;j<pointers_matrix[i].size()-1;j++){
      no_of_printed++;
      if (j==0){
	outs<<"NA";
	print_newline_or_delimiter(outs,no_of_printed,width,delimiter);
	continue;
      }
      if (j==pointers_matrix[i].size()-2){
	outs<<"NA"<<std::endl;
	continue;
      }
      if (j==1){   //first step only covered by one pentamer
	f1_index = 0;
	if (status_matrix[i][f1_index]==0)
	  outs<<"NA";
	else{
	  if (status_matrix[i][f1_index]==1)
	    outs<<std::setprecision(2)<<(*pointers_matrix[i][f1_index]).get_ave(object1);
	  else
	    outs<<std::setprecision(2)<<(*pointers_matrix[i][f1_index]).get_ave(object2);
	}
	print_newline_or_delimiter(outs,no_of_printed,width,delimiter);
	continue;
      }
      if (j==pointers_matrix[i].size()-3){
	f1_index=j-2;
	if (status_matrix[i][f1_index]==0)
	  outs<<"NA";
	else{
	  if (status_matrix[i][f1_index]==1)
	    outs<<std::setprecision(2)<<(*pointers_matrix[i][f1_index]).get_ave(object2);
	  else
	    outs<<std::setprecision(2)<<(*pointers_matrix[i][f1_index]).get_ave(object1);
	}
	print_newline_or_delimiter(outs,no_of_printed,width,delimiter);
	continue;
      }

      f1_index = j-1;
      f2_index = j-2;
      bool flag1=true;
      bool flag2=true;
      if (status_matrix[i][f1_index]==0)
	flag1=false;
      else {
	if (status_matrix[i][f1_index]==1)
	  value1 = (*pointers_matrix[i][f1_index]).get_ave(object1);
	else
	  value1 = (*pointers_matrix[i][f1_index]).get_ave(object2);
      }

      if (status_matrix[i][f2_index]==0)
	flag2=false;
      else{
	if (status_matrix[i][f2_index]==1)
	  value2 = (*pointers_matrix[i][f2_index]).get_ave(object2);
	else
	  value2 = (*pointers_matrix[i][f2_index]).get_ave(object1);
      }
      //std::cout<<flag1<<flag2<<value1<<value2<<std::endl;

      if (flag1 and flag2)
	outs<<std::setprecision(2)<<(value1+value2)/2;
      if (flag1 and (!flag2))
	outs<<std::setprecision(2)<<value1;
      if ((!flag1) and flag2)
	outs<<std::setprecision(2)<<value2;
      if ((!flag1) and (!flag2))
	outs<<"NA";

      print_newline_or_delimiter(outs,no_of_printed,width,delimiter);
    }
  }
}

void print_newline_or_delimiter(std::stringstream &outs,int no_of_printed,int width,char delimiter){
  if ((no_of_printed % width)==0)
    outs<<std::endl;
  else
    outs<<delimiter;
}

void predict_groove_width_inosine(std::string ofilename,string_vector& sequence_list,string_vector& name_list,\
				  DNA_to_properties &pentamers_map,DNA_to_properties &inosine_map){
  std::ofstream outs(ofilename.c_str());

  int f_index;   //the starting position of the pentamer
  for (unsigned int i=0;i<name_list.size();i++){
    outs<<name_list[i]<<std::endl;
    outs<<"NA,NA,";
    outs<< std::fixed;
    for (unsigned int j=2;j<sequence_list[i].size()-2;j++){
      f_index = j-2;
      std::string current_pentamer = sequence_list[i].substr(f_index,5);
      if (current_pentamer.find_first_not_of("ATGCI")!=std::string::npos)
	outs<<"NA,";
      else{
	if (current_pentamer.find("I")!=std::string::npos){  // Inosine detected
	  if (found_str_in_map(current_pentamer,inosine_map))
	    outs<<std::setprecision(2)<<inosine_map[current_pentamer].get_ave("minor")<<",";
	  else
	    outs<<"NA,";
	}
	else{
	  if (found_str_in_map(current_pentamer,pentamers_map))
	    outs<<std::setprecision(2)<<pentamers_map[current_pentamer].get_ave("minor")<<",";
	  else
	    outs<<std::setprecision(2)<<pentamers_map[opposite_strand(current_pentamer)].get_ave("minor")<<",";
	}
      }
    }
    outs<<"NA,NA"<<std::endl;

  }
  outs.close();

}

