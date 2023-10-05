#include "frab.h"

Rcpp::NumericVector values(const frab &F){
  NumericVector out(F.size());
  size_t i=0;
  for(auto it=F.begin(); it != F.end(); ++it){
        out(i++) = it->second; // cf names() below
  }
  return out;
}

Rcpp::CharacterVector names(const frab &F){
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

frab prod2(frab F1, frab F2){
  frab out;
  if(F1.size() > F2.size()){
    for(auto it = F2.begin() ; it != F2.end() ; ++it){ // iterate through the smaller one
      const string symbol = it->first;
      out[symbol] = F1[symbol] * F2[symbol];
    }
    return remove_zeros(out);
  } else { 
    for(auto it = F1.begin() ; it != F1.end() ; ++it){
      const string symbol = it->first;
      out[symbol] = F2[symbol] * F1[symbol];
    }
    return remove_zeros(out);
  }
}

frab frabmaker(const CharacterVector names, const NumericVector values){
  if(names.size() != values.size()) {
    throw std::invalid_argument("names and values are not same length");
  }
  frab out;
  for(size_t i=0 ; i < (size_t) names.size() ; i++){
    if(values[i] != 0){
      out[(string) names[i]] += values[i];  // the meat
    }
  }
  return remove_zeros(out);  // remove_zeros() needed here if, eg, c(a=1,b=3,a=-1)
}

frab pmax(frab F1, frab F2){
  for(auto it = F1.begin() ; it != F1.end() ; ++it){
    const string symbol = it->first;
    F1[symbol] = std::max(F1[symbol],F2[symbol]);
    F2.erase(symbol);
  }

  for(auto it = F2.begin() ; it != F2.end() ; ++it){
    const string symbol = it->first;
    F1[symbol] = std::max(F2[symbol], (double) 0);
  }
    return remove_zeros(F1);
}

List retval(const frab &F){  // used to return a frab to R
  return List::create(Named("names")  =   names(F),
		      Named("values") =  values(F)
		      );
}

bool equal2_samesize(frab F1, frab F2){
  for (auto it=F1.begin(); it != F1.end(); ++it){
      const string symbol = it->first;
      if(F1[symbol] != F2[symbol]){  // meat 1
	return false;
      }
  }
  return true;
}

bool equal(const frab &F1, const frab &F2){
  if(F1.size() != F2.size()){
    return false;
  } else {
    return equal2_samesize(F1,F2);
  }
}



// [[Rcpp::export]]
List c_frab_identity(const CharacterVector names, const NumericVector values){
  return retval(frabmaker(names, values));
}

//[[Rcpp::export]]
List c_frab_add(
	 const CharacterVector names1, const NumericVector values1,
	 const CharacterVector names2, const NumericVector values2
          ){
  return retval(sum2(
		     frabmaker(names1,values1),
		     frabmaker(names2,values2)
		     ) );
}

//[[Rcpp::export]]
List c_frab_multiply(
	 const CharacterVector names1, const NumericVector values1,
	 const CharacterVector names2, const NumericVector values2
          ){
  return retval(prod2(
		     frabmaker(names1,values1),
		     frabmaker(names2,values2)
		     ) );
}

//[[Rcpp::export]]
List c_frab_pmax(
	 const CharacterVector names1, const NumericVector values1,
	 const CharacterVector names2, const NumericVector values2
          ){
  return retval(pmax(
		     frabmaker(names1,values1),
		     frabmaker(names2,values2)
		     ) );
}

//[[Rcpp::export]]
bool c_frab_eq(
	      const CharacterVector names1, const NumericVector values1,
	      const CharacterVector names2, const NumericVector values2
	      ){
  return equal(
	       frabmaker(names1,values1),
	       frabmaker(names2,values2)
	       );
}
