#include <Rcpp.h>
#include "common.h"
#include "utilities.h"
#include "OptionParser.h"
#include "TableCompiler.h"
#include "Output.h"
#include "ReadData.h"
#include "prediction.h"
#include "SNP.h"

using namespace Rcpp;

// [[Rcpp::export]]
std::string getDNAShape(std::string fastaFilePath, std::string shapeType){
  std::ifstream in_fstream(fastaFilePath.c_str());
  std::string outputFile;

  if (!in_fstream){
    Rcout << "Cannot open the input file:  " << fastaFilePath << std::endl;

  }else{

    if(shapeType.compare("MGW")==0 || shapeType.compare("Roll")==0 || shapeType.compare("HelT")==0 ||
      shapeType.compare("ProT")==0 || shapeType.compare("EP")==0){
        Rcout<< "Reading the input sequence......" << std::endl;
        string_vector sequence_list;
        string_vector name_list;
        sequence_list.clear();
        name_list.clear();
        bool debug = false;
        read_fasta(in_fstream, sequence_list, name_list,debug);

        outputFile = fastaFilePath + "." + shapeType;

        //build pentamers map
        DNA_to_properties pentamers_map;
        pentamers_map.clear();
        build_unique_pentamers(pentamers_map);

        //load the query table from file
        //std::string querytable_filename = "QueryTable.dat";
        process_querytable(pentamers_map, debug); //Tsu-Pei

        //convert sequence_list to pointers_list
        std::vector <pointers_vector> pointers_matrix;
        std::vector <int_vector> status_matrix;
        //std::cout << "Indexing the input sequence......"<<std::endl;
        convert_sequence_list(sequence_list,pointers_matrix,status_matrix,pentamers_map);
        //std::cout << "Indexing complete"<<std::endl;

        //run prediction
        std::stringstream current_ss;
        //std::cout << "Processing......"<<std::endl;

        int output_width = 30;
        char delimiter = ',';

        //run MGW
        if(shapeType.compare("MGW")==0){
          current_ss.str("");
          current_ss.clear();
          predict_groove_width(current_ss,pointers_matrix,status_matrix,name_list,debug,pentamers_map,"minor",output_width,delimiter);
          output_stringstream_to_file(current_ss,outputFile);

        //run Roll
        }else if(shapeType.compare("Roll")==0){
          current_ss.str("");
          current_ss.clear();
          predict_step_parameters(current_ss,pointers_matrix,status_matrix,name_list,debug,pentamers_map,"roll",output_width,delimiter);
          output_stringstream_to_file(current_ss,outputFile);

        //run ProT
        }else if(shapeType.compare("ProT")==0){
          current_ss.str("");
          current_ss.clear();
          predict_groove_width(current_ss,pointers_matrix,status_matrix,name_list,debug,pentamers_map,"propel",output_width,delimiter);
          output_stringstream_to_file(current_ss,outputFile);

        //run HelT
        }else if(shapeType.compare("HelT")==0){
          current_ss.str("");
          current_ss.clear();
          predict_step_parameters(current_ss,pointers_matrix,status_matrix,name_list,debug,pentamers_map,"twist",output_width,delimiter);
          output_stringstream_to_file(current_ss,outputFile);

      //run EP
      }else if(shapeType.compare("EP")==0){
        current_ss.str("");
        current_ss.clear();
        predict_groove_width(current_ss,pointers_matrix,status_matrix,name_list,debug,pentamers_map,"ep",output_width,delimiter);
        output_stringstream_to_file(current_ss,outputFile);
      }

      //std::cout << "done"<< std::endl;

    }else{
      Rcout << "Cannot recogize the shape type(MGW/Roll/HelT/ProT):" << shapeType << std::endl;
    }
  }

  return "Output file is in folder of " + outputFile;
}
