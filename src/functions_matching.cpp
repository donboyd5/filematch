// the following rcpp code is based on the commented-out julia code after the function, which was
// translated to c++ by chatGPT and then I fixed some errors by chatGPT




#include <Rcpp.h>
using namespace Rcpp;

//' Good approach
//'
//' Fast matching of records
//' @param adf a dataframe
//' @param bdf a dataframe
//' @return ab dataframe
//' @export
// [[Rcpp::export]]
DataFrame matchrecs(DataFrame adf, DataFrame bdf) {

  // Extracting columns
  IntegerVector ida = adf["ida"];
  NumericVector weighta = adf["weighta"];
  NumericVector ranka = adf["ranka"];

  IntegerVector idb = bdf["idb"];
  NumericVector weightb = bdf["weightb"];
  NumericVector rankb = bdf["rankb"];

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

  return DataFrame::create(_["ida"]=records_ida, _["idb"]=records_idb,
                           _["weight"]=records_weight,
                           _["ranka"]=records_ranka, _["rankb"]=records_rankb);
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

