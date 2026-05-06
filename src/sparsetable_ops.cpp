// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

#define container vector      // Could be 'vector' or 'deque' (both work but there may be performance differences)

#define STRICT_R_HEADERS
#include <Rcpp.h>

#include <string.h>
#include <iostream>
#include <unordered_map>
#include <vector>
#include <deque>
#include <utility>
#include <iterator>


using namespace Rcpp; 

typedef std::container<String> mycont;  // a mycont  is a container [vector or deque] of strings
typedef std::map<mycont, double > sparsetable;

sparsetable prepare(const CharacterMatrix M, const NumericVector d){
    sparsetable S;
    mycont v;

    for(int i=0; i<M.nrow() ; i++){
        if(d[i] != 0){
                v.clear();
                for(int j=0; j<M.ncol(); j++){
                    v.push_back(M(i,j));
                }
                S[v] += d[i];
        }
    }  // i loop closes

    std::erase_if(S, [](const auto& item) {
        return item.second == 0;
    });
    return S;
}

CharacterMatrix makeindex(const sparsetable &S){  // takes a sparsetable, returns the matrix of indices
    if (S.empty()) return CharacterMatrix(0, 0); // Safety first
    
    const size_t ncol = S.begin()->first.size();
    CharacterMatrix out(S.size(), ncol);
    int row = 0;
    
    for (const auto& [key, value] : S) {
        for (size_t col = 0; col < ncol; ++col) {
            out(row, col) = key[col];
        }
        row++;
    }
    return out;
}

NumericVector makevalue(const sparsetable &S){  // takes a sparsetable, returns data
    NumericVector  out(S.size());   // data
    unsigned int i=0;
    sparsetable::const_iterator it;   // it iterates through a sparse array
    
    for(it=S.begin(); it != S.end(); ++it){
        out(i++) = it->second;   // initialize-and-fill is more efficient than  out.push_back(it->second) 
    }
    return out;
}

List retval (const sparsetable &S){  // used to return a list to R

  // In this function, returning a zero-row matrix results in a
  // segfault ('memory not mapped').  So we check for 'S' being zero
  // size and, if so, return a special Nil value.  This corresponds to
  // an empty sparsetable object.
  
    if(S.size() == 0){
        return List::create(Named("index") = R_NilValue,
                            Named("value") = R_NilValue
                            );
    } else {
        return List::create(Named("index") = makeindex(S),
                            Named("value") = makevalue(S)
                            );
    }
}


// [[Rcpp::export]]
List sparsetable_maker
(
  const CharacterMatrix &M, const NumericVector &d
 ){
    return retval(prepare(M,d));
}

// [[Rcpp::export]]
List sparsetable_add
(
 const CharacterMatrix &M1, const NumericVector &d1,
 const CharacterMatrix &M2, const NumericVector &d2
 ){
     sparsetable S1 = prepare(M1, d1);
     sparsetable S2 = prepare(M2, d2);

     for (const auto& [key, val2] : S2) {
         double& val1 = S1[key]; 
         val1 += val2;
         if (val1 == 0) { S1.erase(key); }
     }
     return retval(S1);
}

// [[Rcpp::export]]
List sparsetable_overwrite // something like S1[ind(S2)] <- S2
(
 const CharacterMatrix &M1, const NumericVector &d1,
 const CharacterMatrix &M2, const NumericVector &d2
 ){
    sparsetable S1 = prepare(M1, d1);
    sparsetable S2 = prepare(M2, d2);

    for (const auto& [key, value] : S2) {
        S1.insert_or_assign(key, value);
    }

    return retval(S1);
}

// [[Rcpp::export]]
NumericVector sparsetable_accessor // returns S1[]
(
 const CharacterMatrix &M, const NumericVector &d,
 const CharacterMatrix &Mindex
 ){
    sparsetable S;
    mycont v;
    NumericVector out(Mindex.nrow());
    
    S = prepare(M, d);

    for(int i=0; i<Mindex.nrow() ; i++){
        v.clear();
        for(int j=0; j<Mindex.ncol(); j++){
            v.push_back(Mindex(i,j));
        }

        auto it = S.find(v);
        if (it != S.end()) {
            out[i] = it->second;
        } else {
            out[i] = 0.0; // Key not found, return 0 (standard sparse behavior)
        }
    }
    return out;
}

// [[Rcpp::export]]
List sparsetable_setter // effectively S[M] <- d; return S
(
 const CharacterMatrix &M1, const NumericVector &d1,
 const CharacterMatrix &M2, const NumericVector &d2    // M2 -> index ; d2 -> value
 ){
    mycont v;
    
    sparsetable S1 = prepare(M1, d1);

    for(int i = 0; i < M2.nrow(); i++) {
        v.clear();
        for(int j = 0; j < M2.ncol(); j++) { v.push_back(M2(i, j)); }
        S1.insert_or_assign(v, d2[i]);
    }
    
    std::erase_if(S1, [](const auto& item) {
        auto const& [key, value] = item;
        return value == 0;
    });

    return retval(S1);
}

// [[Rcpp::export]]
bool sparsetable_equality // S1 == S2
(
 const CharacterMatrix &M1, const NumericVector &d1,
 const CharacterMatrix &M2, const NumericVector &d2
 ){
    sparsetable S1 = prepare(M1, d1);
    sparsetable S2 = prepare(M2, d2);

    return S1 == S2;
}

// [[Rcpp::export]]
List sparsetable_asum_include
(
 const CharacterMatrix &M, const NumericVector &d,
 const IntegerVector &n
 ){
    sparsetable S;
    mycont v;

    for(int i=0; i<M.nrow() ; i++){
        v.clear();
        for(int j=0; j<M.ncol(); j++){
            v.push_back(M(i,j));
        }
        for(int k=0 ;  k<n.size() ; k++){
            v[n[k]-1] = 0;    // off-by-one issue dealt with here: if n[k]=0 this means dimension 1.
        }
        S[v] += d[i];
    }
    return retval(S);
}

// [[Rcpp::export]]
List sparsetable_pmax
(
 const CharacterMatrix &M1, const NumericVector &d1,
 const CharacterMatrix &M2, const NumericVector &d2 
 ){
    sparsetable S1 = prepare(M1, d1);
    sparsetable S2 = prepare(M2, d2);

    for (sparsetable::const_iterator it = S1.begin(); it != S1.end(); ++it){
        const mycont v = it->first;
        if(S2[v] > S1[v]){ S1[v] = S2[v];} // S1[v] = max(S1[v],S2[v]);
        S2.erase(v); // not S2[v] = 0;  // OK because the iterator is it1 and this line modifies S2
    }
            
    for (sparsetable::const_iterator it = S2.begin(); it != S2.end(); ++it){ //iterate through S2 keys not in S1
        const mycont v = it->first;
        if(S2[v] > 0){ S1[v] = S2[v]; }
    }

    return retval(S1);
}


