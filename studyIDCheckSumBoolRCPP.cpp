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
	
	// sets up the coidePoint vector
	std::vector<int> codePoint(9, 0);
	
	// sets up the code point list as a string whose characters correspond to
	// code point + 1
	std::string cypher = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*";
	
	// puts the elements of the cypher corresponding to elements in studyID
	// into the codePoint vector
	for (int i = 0; i < 9; i++) {
		std::string j = studyID.substr(i, 1);
		codePoint[i] = cypher.find_first_of(j);
	}
	
	// initialises sum as 0
	int sum = 0;
	
	// working backwards from the final element, multiplies the code point 
	// by 2^n where n is the nth cycle in the iteration then adds to sum
	for (int i = 8; i >= 0; i--) {
		codePoint[i] = codePoint[i] * (int)std::pow( static_cast<double>(2), 
			std::abs(i-9));
		sum += codePoint[i];
	}
	
	// takes the remainder of sum mod base (37 by default)
	int pointCode = (((base + 1) - (sum % base)) % base);
	
	// determines the correct check digit
	std::string checkDigit = cypher.substr(pointCode, 1);
	
	// returns a bool corresponding to computed == actual check digit
	// the last 2 steps can be combined but actually slow things down
	return (checkDigit == studyID.substr(9, 1));
  
}
