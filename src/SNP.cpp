

#include "SNP.h"

//calculate the effect of SNP for all 9-mers

void SNP_distribution(DNA_to_properties &onemap){
  std::string alphabet[4] = {"A","T","G","C"};
  std::string left_flank,right_flank,fragment;
  std::vector<double_vector> matrix;
  double_vector d_vector;
  for (int l1 = 0; l1<4; l1++)
    for (int l2 = 0; l2<4; l2++)
      for (int l3 = 0; l3<4; l3++)
	for (int l4 = 0; l4<4; l4++)
	  for (int r1 = 0; r1<4; r1++)
	    for (int r2 = 0; r2<4; r2++)
	      for (int r3 = 0;  r3<4; r3++)
		for (int r4 = 0; r4<4; r4++){
		  left_flank = alphabet[l1]+alphabet[l2]+alphabet[l3]+alphabet[l4];
		  right_flank = alphabet[r1]+alphabet[r2]+alphabet[r3]+alphabet[r4];
		  matrix.clear();
		  for (int c=0; c<4; c++){
		    fragment = left_flank+alphabet[c]+right_flank;
		    d_vector.clear();
		    predict_mgw_to_vector(onemap,fragment,d_vector);
		    matrix.push_back(d_vector);
		  }
		  //std::cout << std::fixed;
		  for (int c1=0; c1<4; c1++)
		    for (int c2=0; c2<4; c2++)
		      if (c1!=c2){

			//std::cout <<  (left_flank+alphabet[c1]+"/"+alphabet[c2]+right_flank);
			//std::cout << std::setw(8) <<std::setprecision(3) <<pairwise_Euclidean(matrix[c1],matrix[c2]);
			//std::cout << pairwise_Euclidean(matrix,c);
			//Calculate the minor groove width of central position
			//for (int i=0; i<matrix[c].size(); i++)
			//  std::cout << std::setw(7) <<std::setprecision(3) << matrix[c][i];
			//std::cout << std::endl;

		      }
		}
}

void predict_mgw_to_vector(DNA_to_properties &onemap,std::string fragment,double_vector &d_vector){
  d_vector.clear();
  std::string pentamer;
  for (unsigned int i=2;i<fragment.size()-2;i++){
    pentamer=fragment.substr(i-2,5);
    if (found_str_in_map(pentamer,onemap))
      d_vector.push_back(onemap[pentamer].get_ave("minor"));
    else
      d_vector.push_back(onemap[opposite_strand(pentamer)].get_ave("minor"));
  }
}

double pairwise_Euclidean(double_vector &v1,double_vector &v2){
  double sum;
  int len = v1.size();
  sum = 0;
  for (int k=0;k<len;k++)
    sum +=  pow(v1[k]-v2[k],2);
  sum = sqrt(sum);
  return sum;

}
