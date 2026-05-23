#include "frab.h"

Rcpp::NumericVector values(const frab &F){
  Rcpp::NumericVector out = Rcpp::no_init(F.size());
  size_t i=0;
  for (const auto& item : F) {
    out[i++] = item.second;
  }
  return out;
}

Rcpp::CharacterVector names(const frab &F){
  Rcpp::CharacterVector out = Rcpp::no_init(F.size());
  size_t i=0;
  for(auto it=F.begin(); it != F.end(); ++it){
    out[i++] = it->first;  // cf power() above
  }
  return out;
}

frab remove_zeros(frab &F){// might be better to call this "nonzero_entries()"
  frab out;
  for(const auto& [symbol, power] : F){
    if(power != 0){
      out.emplace(symbol, power);
    }
  }
  return out;
}

frab sum2(frab F1, frab F2){
  if(F1.size() > F2.size()){
    for(const auto& [symbol, power] : F2 ){
      F1[symbol] += power;
    }
    return remove_zeros(F1);
  } else { 
    for(const auto& [symbol, power] : F1 ){
      F2[symbol] += power;
    }
    return remove_zeros(F2);
  }
}

frab prod2(frab F1, frab F2){
  frab out;

  const frab &smaller = (F1.size() < F2.size()) ? F1 : F2;
  const frab &larger  = (F1.size() < F2.size()) ? F2 : F1;
  
  for(const auto& [symbol, power] : smaller){
    auto it = larger.find(symbol);

    if(it != larger.end()){
      double result = power * it->second;
      auto it = larger.find(symbol);
      if(result != 0){
	out.emplace(symbol, result);
      }
    }
  }
  return remove_zeros(out);
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

bool equal2_samesize(const frab &F1, const frab &F2){
  auto it1 = F1.begin();
  auto it2 = F2.begin();

  for (; it1 !=F1.end(); ++it1, ++it2){
    if( (it1->first != it2->first) || (it1->second != it2->second)){
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
