#include "map.h"
#include "utilities.h"

void build_unique_pentamers(DNA_to_properties& pentamers){
  std::string alphabet[4]={"A","T","G","C"};
  pentamers.clear();
  std::string t="";
  for (int i=0;i<4;i++)
    for (int j=0;j<4;j++)
      for (int k=0;k<4;k++)
	for (int l=0;l<4;l++)
	  for (int m=0;m<4;m++){
	    t=alphabet[i]+alphabet[j]+alphabet[k]+alphabet[l]+alphabet[m];
	    if ((!found_str_in_map(t,pentamers)) && (!found_str_in_map(opposite_strand(t),pentamers))){
	      properties p=properties();
	      pentamers[t]=p;
	    }
	  }
}

bool found_str_in_map(std::string str, DNA_to_properties &onemap){
  DNA_to_properties::const_iterator end=onemap.end();
  if (onemap.find(str)!=end)
    return true;
  else 
    return false;
}
