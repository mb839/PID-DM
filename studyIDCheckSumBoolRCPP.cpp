# include <iostream>
# include <algorithm>
# include <vector>
# include <cmath>
# include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]

bool studyCheckSum(std::string studyID) {
  
  // checks that studyID has a length of 10
  if (studyID.length() != 10) {
    return false;
  }
  
  // sets up the base as 37, this can be changed if needed
  int base = 37;
  
  // std::string input2 = studyID.substr(0, 9);
  
  // sets up the coidePoint vector
  std::vector<int> codePoint(9, 0);
  // sets up the code point list as a string whose characters correspond to
  // code point + 1
  std::string cypher = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*";
  
  for (int i = 0; i < 9; i++) {
    std::string j = studyID.substr(i, 1);
    codePoint[i] = cypher.find_first_of(j);
  }
  
  int sum = 0;
  
  for (int i = 9; i >= 0; i--) {
    codePoint[i] = codePoint[i] * std::pow(2, std::abs(i-9));
    sum += codePoint[i];
  }
  
  int pointCode = (((base + 1) - (sum % base)) % base);
  
  sum = 0;
  
  std::string checkDigit = cypher.substr(pointCode, 1);
  
  return (checkDigit == studyID.substr(9, 1));
  
}
