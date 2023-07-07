#include "frab.h"

Rcpp::NumericVector powers(const frab &F){
  NumericVector out(F.size());
  size_t i=0;
  for(auto it=F.begin(); it != F.end(); ++it){
        out(i++) = it->second; // cf symbols() below
  }
  return out;
}

Rcpp::CharacterVector symbols(const frab &F){
  CharacterVector out(F.size());
  size_t i=0;
  for(auto it=F.begin(); it != F.end(); ++it){
    out(i++) = it->first;  // cf power() above
  }
  return out;
}

frab remove_zeros(frab F){// might be better to call this "nonzero_entries()"
  frab out;
  for(frab::iterator it = F.begin() ; it != F.end() ; ++it){
    const string symbol = it->first;
    const double power =  it->second;
    if(power != 0){
      out[symbol] = power; // the meat
    }
  }
  return out;
}

frab sum2(frab F1, frab F2){
  if(F1.size() > F2.size()){
    for(auto it = F2.begin() ; it != F2.end() ; ++it){ // iterate through the smaller one
      const string symbol = it->first;
      F1[symbol] += F2[symbol];
    }
    return remove_zeros(F1);
  } else { 
    for(auto it = F1.begin() ; it != F1.end() ; ++it){
      const string symbol = it->first;
      F2[symbol] += F1[symbol];
    }
    return remove_zeros(F2);
  }
}

frab frabmaker(const CharacterVector symbols, const NumericVector powers){
  if(symbols.size() != powers.size()) {
    throw std::invalid_argument("symbols and powers are not same length");
  }
  frab out;
  for(size_t i=0 ; i < (size_t) symbols.size() ; i++){
    if(powers[i] != 0){
      out[(string) symbols[i]] += powers[i];  // the meat
    }
  }
  return remove_zeros(out);  // remove_zeros() needed here if, eg, c(a=1,b=3,a=-1)
}

List retval(const frab &F){  // used to return a frab to R
  return List::create(Named("symbols") =  symbols(F),
		      Named("powers")  =  powers (F)
		      );
}

bool equal2_samesize(frab F1, frab F2){
  for (auto it=F1.begin(); it != F1.end(); ++it){
      const string symbol = it->first;
      if(F1[symbol] != F2[symbol]){  // meat 1
	return false;
      } else {
	F2.erase(symbol);
      }
  }
  return F2.empty();
}

bool equal(const frab &F1, const frab &F2){
  if(F1.size() != F2.size()){
    return false;
  } else {
    return equal2_samesize(F1,F2);
  }
}


// [[Rcpp::export]]
List c_frab_identity(const CharacterVector symbols, const NumericVector powers){
  return retval(frabmaker(symbols, powers));
}

//[[Rcpp::export]]
List c_frab_add(
	 const CharacterVector symbols1, const NumericVector powers1,
	 const CharacterVector symbols2, const NumericVector powers2
          ){
  return retval(remove_zeros(sum2(
			     frabmaker(symbols1,powers1),
			     frabmaker(symbols2,powers2)
				  ) ) );
}

//[[Rcpp::export]]
bool c_frab_eq(
	      const CharacterVector symbols1, const NumericVector powers1,
	      const CharacterVector symbols2, const NumericVector powers2
	      ){
  return equal(
	       frabmaker(symbols1,powers1),
	       frabmaker(symbols2,powers2)
	       );
}
