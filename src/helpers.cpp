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
NumericVector parse_num(CharacterVector x) {
  int l = x.size();
  NumericVector result(l);
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
      result[i] = NA_REAL;
    }else{
      result[i] = std::stod(s);
    }
  }
  return result;
}

//' Convert id to site location
//'
//' Convert studyid to site location
//'
//' @param id studyid
//' @return string of site location or NA
//' @export
//' @import tidyverse
// [[Rcpp::export]]
CharacterVector parse_site_c(IntegerVector id) {
  int l = id.size();
  CharacterVector site(l);
  for(int i = 0; i < l; i++){  
    if(IntegerVector::is_na(id[i])){
      site[i] = NA_STRING;
    }else{
      switch(id[i]/10000){
        case 1:
          site[i] = "001-Beijing Chaoyang Hospital";
          break;
        case 2:
          site[i] = "002-Langfang People's Hospital";
          break;
        case 5:
          site[i] = "005-Langfang Aidebao General Hospital";
          break;
        case 7:
          site[i] = "007-Second Bethune Hospital of Jilin University";
          break;
        case 8:
          site[i] = "008-Tianjin Hospital";
          break;
        case 9:
          site[i] = "009-Beijing Anzhen Hospital";
          break;
        case 10:
          site[i] = "010-Harbin Medical University Second Hospital";
          break;
        case 11:
          site[i] = "011-Shenyang Orthopaedic Hospital";
          break;
        case 13:
          site[i] = "013-Hanzhong People's Hospital";
          break;
        case 14:
          site[i] = "014-Shanghai No.10 People's Hospital";
          break;
        case 15:
          site[i] = "015-Xiamen University affiliated First Hospital";
          break;
        case 16:
          site[i] = "016-The 2nd affiliated hospital of Wenzhou Medical University";
          break;
        case 101:
          site[i] = "101-Mulago Hospital, Uganda";
          break;
        case 102:
          site[i] = "102-Rift Valley Provincial General Hospital, Kenya";
          break;
        case 103:
          site[i] = "103-Kenyatta National Hospital, Kenya";
          break;
        case 104:
          site[i] = "104-REMOVED Muhimbili Orthopaedic Institute";
          break;
        case 106:
          site[i] = "106-Kiambu District Hospital, Kenya";
          break;
        case 108:
          site[i] = "108-KCMC - Kilimanjaro Christian Medical Centre, Tanzania";
          break;
        case 109:
          site[i] = "109-Chris Hani Baragwanath Hospital, South Africa";
          break;
        case 110:
          site[i] = "110-Charlotte Maxeke Johannesburg Academic Hospital, South Africa";
          break;
        case 111:
          site[i] = "111-Helen Joseph Hospital, South Africa";
          break;
        case 112:
          site[i] = "112-AIC Kijabe Hospital, Kenya";
          break;
        case 113:
          site[i] = "113-REMOVED Ondo State Trauma and Surgical Centre";
          break;
        case 114:
          site[i] = "114-Princess Marina Hospital, Botswana";
          break;
        case 115:
          site[i] = "115-Black Lion Hospital, Addis Ababa, Ethiopia";
          break;
        case 117:
          site[i] = "117-KATH, Kumasi, Ghana";
          break;
        case 120:
          site[i] = "120-National Orthopedic Hospital, Enugu, Nigeria";
          break;
        case 122:
          site[i] = "122-Baptist Hospital Mutengene, Cameroon";
          break;
        case 201:
          site[i] = "201-Sancheti Institute of Orthopaedics";
          break;
        case 204:
          site[i] = "204-Noble Hospital";
          break;
        case 205:
          site[i] = "205-Bharati Vidyapeeth University Medical College";
          break;
        case 206:
          site[i] = "206-Datta Meghe Institute of Medical Sciences";
          break;
        case 208:
          site[i] = "208-AIIMS";
          break;
        case 209:
          site[i] = "209-CMC Vellore";
          break;
        case 210:
          site[i] = "210-CMC Ludhiana";
          break;
        case 211:
          site[i] = "211-Indian Institute for Spinal Care";
          break;
        case 212:
          site[i] = "212-IGMC & RI";
          break;
        case 213:
          site[i] = "213-St. John's Medical Colle";
          break;
        case 214:
          site[i] = "214-Post Graduate Institute of Medical Education and Research";
          break;
        case 215:
          site[i] = "215-Baptist Christian Hospital";
          break;
        case 216:
          site[i] = "216-NHL Medical College, Ahmedabad";
          break;
        case 301:
          site[i] = "301-Northwest General Hospital & Research, Pakistan";
          break;
        case 302:
          site[i] = "302-Lumbini Medical College, Nepal";
          break;
        case 303:
          site[i] = "303-Jigme Dorji Wangchuck National Referral Hospital";
          break;
        case 305:
          site[i] = "305-Cho Ray Hospital, Vietnam";
          break;
        case 306:
          site[i] = "306-Viet Duc Hospital, Vietnam";
          break;
        case 307:
          site[i] = "307-Ramathibodi Hospital, Thailand";
          break;
        case 309:
          site[i] = "309-Khon Kaen Hospital, Thailand";
          break;
        case 310:
          site[i] = "310-Philippine General Hospital, Manila, Philippines";
          break;
        case 311:
          site[i] = "311-Sina Trauma and Surgery Research Center";
          break;
        case 401:
          site[i] = "401-Hospital Civil de Guadalajara, Mexico";
          break;
        case 402:
          site[i] = "402-Hospital Universitario de Caracas, Venezuela";
          break;
        case 403:
          site[i] = "403-Hospital Central de IPS, Paraguay";
          break;
        case 404:
          site[i] = "404-Hospital Sirio Libanes, Buenos Aires, Argentina";
          break;
        case 405:
          site[i] = "405-Hospital Puerto de Hierro, Zapopan, Mexico";
          break;
        case 406:
          site[i] = "406-Hospital de Clinicas - UNICAMP, Brazil";
          break;
        case 407:
          site[i] = "407-Clinica Zabala, Buenos Aires, Argentina";
          break;
        case 408:
          site[i] = "408-Ruth Paz Foundation, Honduras";
          break;
        case 409:
          site[i] = "409-Centro Medio Imbanaco, Colombia";
          break;
        case 410:
          site[i] = "410-Clinica Fracturas Y Fracturas, Columbia";
          break;
        default:
          site[i] = NA_STRING;
          break;
      }
    }
  }
  return site;
}