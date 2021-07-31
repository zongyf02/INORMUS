#include <Rcpp.h>
#include <string>
using namespace Rcpp;

//' Convert string to number
//'
//' M will be converted to -2
//' * (lost) will be converted to -3
//' N will be converted to -1
//'
//' @param x string
//' @return integer
//' @export
//' @useDynLib INORMUS, .registration = TRUE
//' @importFrom Rcpp sourceCpp
//' @import tidyverse
// [[Rcpp::export]]
IntegerVector parse_num_c(CharacterVector x) {
  int l = x.size();
  IntegerVector result(l);
  for(int i = 0; i<l; ++i){
    std::string s;
    s = x[i];
    if(s == "M"){
      result[i] = -2;
    }else if(s == "*"){
      result[i] = -3;
    }else if(s == "N"){
      result[i] = -1;
    }else if(CharacterVector::is_na(x[i])){
      result[i] = NA_INTEGER;
    }else{
      result[i] = std::stoi(s);
    }
  }
  return result;
}
