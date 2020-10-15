#include <iostream>
#include <alps/alea.h>
#include <boost/random.hpp> 

int main()
{
 //DEFINE RANDOM NUMBER GENERATOR
 typedef boost::minstd_rand0 random_base_type;
 typedef boost::uniform_01<random_base_type> random_type;
 random_base_type random_int;
 random_type random(random_int);

 //DEFINE OBSERVABLE
 alps::RealObservable obs_a("observable a");

 //ADD 1000 MEASUREMENTS TO THE OBSERVABLE
 for(int i = 0; i < 1000; ++i){ 
   obs_a << random();
 }

 //RESET OBSERVABLES (THERMALIZATION FINISHED)
 obs_a.reset(true);

 //ADD 10000 MEASUREMENTS TO THE OBSERVABLE
 for(int i = 0; i < 10000; ++i){
   obs_a << random();
 }

 //OUTPUT OBSERVABLE
 std::cout << obs_a;       
}