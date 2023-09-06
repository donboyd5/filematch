// the following rcpp code is based on the commented-out julia code after the function, which was
// translated to c++ by chatGPT and then I fixed some errors by chatGPT


#include <Rcpp.h>
using namespace Rcpp;

//' Good approach
//'
//' Fast matching of records
//' @param adf dataframe with A records
//' @param bdf dataframe with B records
//' @param vida string, A id variable name default "ida"
//' @param vidb string, b id variable name default "idb"
//' @param vweighta string, A weight variable name default "weighta"
//' @param vweightb string, B weight variable name default "weightb"
//' @param vranka string, A rank variable name default "ranka"
//' @param vrankb string, B rank variable name default "rankb"
//' @return ab dataframe
//' @export
// [[Rcpp::export]]
DataFrame internal_matchrecs(DataFrame adf, DataFrame bdf,
                             std::string vida = "ida", std::string vidb = "idb",
                             std::string vweighta = "weighta", std::string vweightb = "weightb",
                             std::string vranka = "ranka", std::string vrankb = "rankb") {

  // Extracting columns
  IntegerVector ida = adf[vida];
  NumericVector weighta = adf[vweighta];
  NumericVector ranka = adf[vranka];

  IntegerVector idb = bdf[vidb];
  NumericVector weightb = bdf[vweightb];
  NumericVector rankb = bdf[vrankb];

  int ia = 0; // 0-based in C++
  int ib = 0;

  double tmp_weighta = weighta[0]; // seed with info from the first record
  double tmp_weightb = weightb[0];

  std::vector<int> records_ida, records_idb;
  std::vector<double> records_weight, records_ranka, records_rankb;

  while (true) {
    if (tmp_weightb > tmp_weighta) { // done with the a record so write result
      records_ida.push_back(ida[ia]);
      records_idb.push_back(idb[ib]);
      records_weight.push_back(tmp_weighta);
      records_ranka.push_back(ranka[ia]);
      records_rankb.push_back(rankb[ib]);

      tmp_weightb = tmp_weightb - tmp_weighta;
      ia++;
      if(ia >= ida.size()){
        break;
      }
      tmp_weighta = weighta[ia]; // get the next weighta

    } else {
      records_ida.push_back(ida[ia]);
      records_idb.push_back(idb[ib]);
      records_weight.push_back(tmp_weightb);
      records_ranka.push_back(ranka[ia]);
      records_rankb.push_back(rankb[ib]);

      tmp_weighta = tmp_weighta - tmp_weightb;
      ib++;
      if(ib >= idb.size()){
        break;
      }
      tmp_weightb = weightb[ib]; // get the next weightb
    }
  }

  return DataFrame::create(_[vida]=records_ida, _[vidb]=records_idb,
                           _["weight"]=records_weight,
                           _[vranka]=records_ranka, _[vrankb]=records_rankb);
}



/*
Here's the julia code that the cpp function is based on

function matchrecs(adf, bdf)
# adf and bdf are dataframes with the following columns, all of which are numeric
# adf: ida (integer), weighta (float), ranka (float)
# bdf: idb (integer), weightb (float), rankb (float)

 sort!(adf, :ranka)
 sort!(bdf, :rankb)

 ia = 1
 ib = 1
 records = []
 weighta = adf[1, :weighta]
 weightb = bdf[1, :weightb]
 while true
 if weightb > weighta # we can only use part of weight b on this a rec
# note that we use weightb=weighta -- we can only use that amount
 push!(records, (ida=adf[ia, :ida], idb=bdf[ib, :idb], weighta=weighta, weightb=weighta, ranka=adf[ia, :ranka], rankb=bdf[ib, :rankb]))
 weightb = weightb - weighta # reduce weightb by the amount just written
 ia += 1 # done with this arec, get a new one
 if ia > nrow(adf)
 break
 end
 weighta = adf[ia, :weighta]
 elseif weightb <= weighta
# use as much as we can
 push!(records, (ida=adf[ia, :ida], idb=bdf[ib, :idb], weighta=weightb, weightb=weightb, ranka=adf[ia, :ranka], rankb=bdf[ib, :rankb]))
 weighta = weighta - weightb # reduce weightb by the amount just written
 ib += 1 # done with this brec, get a new one
 if ib > nrow(bdf)
 break
 end
 weightb = bdf[ib, :weightb]
 end
 end
 return DataFrame(records)
 end

 */

