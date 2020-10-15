// rng.cpp
// Test generators in rng.h

#include <iostream>
#include <fstream>
#include <cmath>
#include <cstdlib>

using namespace std;

#include "rng.h"

double badran ( ) {
    int m = 6075;
    int a = 106;
    int c = 1283;
    static int seed = 0;
    seed = (a * seed + c) % m;
    return seed/double(m);
}

int main ( ) {
    cout << "Enter number of points: ";
    int N;
    cin >> N;
    ofstream file("ran2.dat");
    cout << "Testing ran2 ..." << flush;
    int idum = -123456789;
    for (int i = 0; i < N; i++)
	file << ran2(idum) << '\t' << ran2(idum) << '\n';
    file.close();
    cout << "\nTesting qadran ..." << flush;
    file.open("qadran.dat");
    for (int i = 0; i < N; i++)
	file << qadran() << '\t' << qadran() << '\n';
    file.close();
    cout << "\nTesting rand ..." << flush;
    file.open("rand.dat");
    for (int i = 0; i < N; i++)
	file << rand()/double(RAND_MAX) << '\t'
	     << rand()/double(RAND_MAX) << '\n';
    file.close();
    cout << "\nTesting badran ..." << flush;
    file.open("badran.dat");
    for (int i = 0; i < N; i++)
	file << badran() << '\t' << badran() << '\n';
    file.close();
    cout << "\nDone" << endl;
}
