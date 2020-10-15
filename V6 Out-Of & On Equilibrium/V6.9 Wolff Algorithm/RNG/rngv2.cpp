#include <iostream>
#include <cmath>
#include <random>
#include <fstream>
using namespace std;

int main()
{
	// declare a random number generator
	// here we choose the merton twister engine (32bit)
	mt19937 rng;
	// a uniform distribution
	uniform_real_distribution<double> U(0.,1.);
	ofstream file("aleat.dat");
	// get a number from the uniform distribution
	for (int i = 0; i < 100; ++i)
	file << U(rng)*20 << '\t' << U(rng)*20 << endl;
	file.close();
	return 0;
}