// Wolff cluster algorithm for the 2-D Ising Model

#include <cmath>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <list>
#include "rng.h"

using namespace std;

double J = +1;                  // ferromagnetic coupling
int Lx, Ly;                     // number of spins in x and y
int N;                          // number of spins
int **s;                        // the spins
double T,Tmax,Tmin,Tstep;       // temperature
double H = 0;                   // magnetic field
int steps;                      // number of Monte Carlo steps
int idum = -123456789;          // seed
double alpha[3][3];
double alphaud;

void initializeAlpha () {

    alpha [0][0] = 1;
    alpha [2][2] = 1;
    alpha [0][2] = alpha [2][0] = alphaud;
}

void initialize ( ) {
    s = new int* [Lx];
    for (int i = 0; i < Lx; i++)
        s[i] = new int [Ly];
    for (int i = 0; i < Lx; i++)
        for (int j = 0; j < Ly; j++)
            s[i][j] = ran2(idum) < 0.5 ? +1 : -1;   // hot start
    steps = 0;
}

bool **cluster;                     // cluster[i][j] = true if i,j belongs
double addProbability;              // 1 - e^(-2J/kT)

void initializeClusterVariables() {

    // allocate 2-D array for spin cluster labels
    cluster = new bool* [Lx];
    for (int i = 0; i < Lx; i++)
        cluster[i] = new bool [Ly];

    // compute the probability to add a like spin to the cluster
    addProbability = 1 - exp(-J*(alphaud + 1)/double(T));    
    
}

// declare functions to implement Wolff algorithm
void growCluster(int i, int j, int clusterSpin);
void tryAdd(int i, int j, int clusterSpin);

void oneMonteCarloStep() {
    //cout << "Entered\n" ;
    // no cluster defined so clear the cluster array
    for (int i = 0; i < Lx; i++)
    for (int j = 0; j < Lx; j++)
        cluster[i][j] = false;
    //cout << "Cluster value: " << cluster[0][0] << endl;
    // choose a random spin and grow a cluster
    int i = int(ran2(idum) * Lx);
    //cout << "Posx: " << ran2(idum) << endl;
    int j = int(ran2(idum) * Ly);
    //cout << "current Pos:" <<  i << ":" << j << "value" << s[i][j] << endl;
    growCluster(i, j, s[i][j]);

    ++steps;
}

void growCluster(int i, int j, int clusterSpin) {

    // mark the spin as belonging to the cluster and flip it
    cluster[i][j] = true;
    s[i][j] = -s[i][j];

    // find the indices of the 4 neighbors
    // assuming periodic boundary conditions
    int iPrev = i == 0    ? Lx-1 : i-1;
    int iNext = i == Lx-1 ? 0    : i+1;
    int jPrev = j == 0    ? Ly-1 : j-1;
    int jNext = j == Ly-1 ? 0    : j+1;

    // if the neighbor spin does not belong to the
    // cluster, then try to add it to the cluster
    if (!cluster[iPrev][j])
        tryAdd(iPrev, j, clusterSpin);
    if (!cluster[iNext][j])
        tryAdd(iNext, j, clusterSpin);
    if (!cluster[i][jPrev])
        tryAdd(i, jPrev, clusterSpin);
    if (!cluster[i][jNext])
        tryAdd(i, jNext, clusterSpin);
}

void tryAdd(int i, int j, int clusterSpin) {
    if (s[i][j] == clusterSpin)
        if (ran2(idum) < addProbability){
            //printf("Agregado..\n");
            growCluster(i, j, clusterSpin);
        }
}

// variables to measure chi and its error estimate
double chi;                // current susceptibility per spin
double chiSum;             // accumulate chi values
double chiSqdSum;          // accumulate chi^2 values
int nChi;                  // number of values accumulated
double chiAbs;

// energy variables
double E;                  // current energy
double Esum;               // sum of energy
double E2sum;              // sum of energy squared

// magnetization variables
double M;
double MabsSum;
double M2sum;
double M4sum;

// anti magnet variables
double aM,aMabsSum;
double aM2sum,aM4sum;

// variables to measure the heat capacity and its error estimate
double heatCap;            // current heat capactiy per spin
double heatCapSum;         // accumulate chi values
int nheatCap;              // number of values accumulated

// variables to measure autocorrelation time
int nSave = 10;            // number of values to save
double cChiSum;            // accumulate
list<double> chiSave;      // the saved values
double *cChi;              // correlation sums
int nCorr;                 // number of values accumulated

// variables to estimate fluctuations by blocking
int stepsPerBlock = 1000;  // suggested in Wolff paper
double chiBlock;           // used to calculate block average
double chiBlockSum;        // accumulate block <chi> values
double chiBlockSqdSum;     // accumulate block <chi>^2 values
int stepInBlock;           // number of steps in current block
int blocks;                // number of blocks

void initializeObservables() {
    // Initialize chi observables
    chiSum = chiSqdSum = 0;
    nChi = 0;
    chiBlock = chiBlockSum = chiBlockSqdSum = 0;
    stepInBlock = blocks = 0;
    cChiSum = 0;
    cChi = new double [nSave + 1];
    for (int i = 0; i <= nSave; i++)
        cChi[i] = 0;
    nCorr = 0;
    // Initialize heat capacity observables
    heatCapSum = 0;
    nheatCap = 0;
    // Initialize energy observables
    Esum = E2sum = 0;
    // Initialize magnetization observables
    MabsSum = M2sum = M4sum = 0;
    // initialize anti magnet obser
    aMabsSum = aM2sum = aM4sum = 0;
}

void measureObservables() {
    // observables are derived from the magnetic moment
    // & energy derived observables
    E = 0;
    M = 0;
    int black = 0, white = 0;
    double magnetBlack = 0, magnetWhite=0;
    for (int i = 0; i < Lx; i++){
        //printf("\n");
        for (int j = 0; j < Ly; j++){
            // Magnetization
            M += s[i][j]; 
            /*if (s[i][j] == 1)
            printf("*");
            else
            printf(" ");  */ 
            // PBC
            // find the indices of the 4 neighbors
            // assuming periodic boundary conditions
            int iPrev = i == 0    ? Lx-1 : i-1;
            int iNext = i == Lx-1 ? 0    : i+1;
            int jPrev = j == 0    ? Ly-1 : j-1;
            int jNext = j == Ly-1 ? 0    : j+1;
            // Energy
             E += - J * s[i][j] * ( alpha[s[i][j]+1][s[iPrev][j]+1]* s[iPrev][j] + alpha[s[i][j]+1][s[iNext][j]+1]*s[iNext][j] +\
             alpha[s[i][j]+1][s[i][jPrev]+1]*s[i][jPrev] + alpha[s[i][j]+1][s[i][jNext]+1]*s[i][jNext] );   
            if ( ( (i%2 == 0) && (j%2 == 0) ) || ( (i%2 != 0) && (j%2 != 0) ) ) {
                //printf("%d %d\n",i,j );
                magnetBlack += s[i][j];
                black++;
                //printf("black: %d %d %d %d\n",black,i,j,s[i][j] );
            } else{
                magnetWhite += s[i][j];
                white++;
                //printf("white: %d %d %d %d\n",white,i,j,s[i][j] );
            } //end if
        }    
    }
    //Divide energy by two
    E = E/double(2);
        
    chi = M * double(M) / double(N); // <M^2>
    // accumulate values
    chiSum += chi;
    chiSqdSum += chi * chi;
    ++nChi;
    Esum += E;
    E2sum += E*E;
    MabsSum += abs(M);
    M2sum += M*M;
    M4sum += M*M*M*M;

    // anti magnet quantities
    magnetBlack = magnetBlack / double(black);
    magnetWhite = magnetWhite / double(white);

    aM = abs(magnetBlack - magnetWhite) / double(2);
    //printf("\nnew black: %f  white : %f aM: %f\n", magnetBlack, magnetWhite,aM);
    aMabsSum += aM;
    aM2sum += aM*aM;
    aM4sum += aM*aM*aM*aM;
    //printf("%f %f %f %f\n",aM,aMabsSum,aM2sum,aM4sum );
    //cin.get();
    
    // --------- ERRORS -----------
    // accumulate correlation values
    if (chiSave.size() == nSave) {
        cChiSum += chi;
        cChi[0] += chi * chi;
        ++nCorr;
        list<double>::const_iterator iter = chiSave.begin();
        for (int i = 1; i <= nSave; i++)
            cChi[i] += *iter++ * chi;
        chiSave.pop_back();     // remove oldest saved chi value
    }
    chiSave.push_front(chi);    // add current chi value
    // accumulate block values
    chiBlock += chi;
    ++stepInBlock;
    if (stepInBlock == stepsPerBlock) {
        chiBlock /= stepInBlock;
        chiBlockSum += chiBlock;
        chiBlockSqdSum += chiBlock * chiBlock;
        ++blocks;
        stepInBlock = 0;
        chiBlock = 0;
    }
}

// averages of observables
double chiAve;               // average susceptibility per spin
double Eave;                 // average energy per spin
double heatCapAve;           // average heat capacity per spin
double MabsAve;              // average absolute magnetization per spin
double cumAve;               // average cumulant
double chiAbsAve;            // average abs susceptibility per spin
double aMabsAve;
double achiAbsAve;
double acumAve;
double chiError;             // Monte Carlo error estimate
double chiStdDev;            // Standard deviation error from blocking
double tauChi;               // autocorrelation time
double tauEffective;         // effective autocorrelation time

void computeAverages()  {

    // average susceptibility per spin
    chiAve = chiSum / nChi / T;
    Eave = Esum / nChi / N;
    heatCapAve = ( E2sum/nChi - pow((Esum/nChi),2) )/double(N)/pow(T,2);  // beta^2*(<E^2> - <E>^2)/N
    MabsAve = MabsSum / nChi / double(N);
    chiAbsAve = ( M2sum/nChi - pow((MabsSum/nChi),2) ) / double(N)/T;
    cumAve = 1 - ( M4sum/nChi / (3*pow(M2sum/nChi,2)) ); // 1 - <M^4>/(3*<M^2>^2)
    // anti magnet
    aMabsAve = aMabsSum / nChi; // double(N);
    achiAbsAve = ( aM2sum/nChi - pow((aMabsSum/nChi),2) )/T; // double(N)/T;
    acumAve = 1 - ( aM4sum/nChi / (3*pow(aM2sum/nChi,2)) ); 

    // Monte Carlo error estimate
    chiError = chiSqdSum / nChi;
    chiError = sqrt(chiError - chiAve * chiAve);
    chiError /= sqrt(double(nChi));
    // exponential correlation time
    tauChi = 0;
    double cAve = cChiSum / nCorr;
    double c0 = cChi[0] / nCorr - cAve * cAve;
    for (int i = 1; i <= nSave; i++) {
         double c = (cChi[i] / nCorr - cAve * cAve) / c0;
         if (c > 0.01) {
             tauChi += -i/log(c);
         } else {
             tauChi /= (i - 1);
             break;
         }
         if (i == nSave)
             tauChi /= nSave;
    }
    // standard deviation from blocking
    double chiBlockAve = chiBlockSum / blocks;
    chiStdDev = chiBlockSqdSum / blocks;
    chiStdDev = sqrt(chiStdDev - chiBlockAve * chiBlockAve);
    chiStdDev /= sqrt(double(blocks));
    // effective autocorrelation time
    tauEffective = chiStdDev / chiError;
    tauEffective *= tauEffective / 2;
}

int main() {

    cout << " Two-dimensional Ising Model - Wolff Cluster Algorithm\n"
         << " -----------------------------------------------------\n"
         << " Enter number of spins L in each direction: ";
    cin >> Lx;
    Ly = Lx;
    N = Lx * Ly;
    cout << " Enter MAXIMUM temperature T: ";
    cin >> Tmax;
    cout << " Enter MINIMUM temperature T: ";
    cin >> Tmin;
    cout << " Enter STEP temperatures: ";
    cin >> Tstep;
    cout << " Enter number of Monte Carlo steps: ";
    int MCSteps;
    cin >> MCSteps;
    cout << " Enter alphaud a: ";
    cin >> alphaud;

    initializeAlpha();
    int thermSteps = MCSteps / 5;

    int numSteps = int( (Tmax - Tmin )/Tstep);
    for (int t = 0; t <= numSteps; ++t)
    {
        T = Tmax - t*Tstep;
        
        cout << "current temperature: " << T << endl;
        
        //cout << s[19][19] << "\n";
        initialize();
        initializeClusterVariables();
        /*cout << " Performing " << thermSteps 
             << " thermalization steps ..." << flush;*/
        for (int i = 0; i < thermSteps; i++)
            oneMonteCarloStep();

        //cout << " done\n Performing production steps ..." << flush;
        initializeObservables();
        for (int i = 0; i < MCSteps; i++) {
            oneMonteCarloStep();
            measureObservables();
        }
        //cout << " done" << endl;
        computeAverages();
        
        /*cout << "\n       Average chi per spin = " << chiAve
             << "\n Monte Carlo error estimate = " << chiError
             << "\n   Autocorrelation time tau = " << tauChi
             << "\n   Std. Dev. using blocking = " << chiStdDev
             << "\n              Effective tau = " << tauEffective 
             << "\n    Average energy per spin = " << Eave 
             << "\n  Average heat cap per spin = " << heatCapAve 
             << "\nAverage abs magnet per spin = " << MabsAve 
             << "\n   Average abs chi per spin = " << chiAbsAve
             << "\n           Average cumulant = " << cumAve << endl; */
        ofstream file("Output.dat", ofstream::app); // append
        file << Lx << '\t' << alphaud << '\t' << T << '\t' << chiAve << '\t' << chiError << '\t' << tauChi << '\t' << chiStdDev << '\t' << tauEffective \
             << '\t' << Eave << '\t' << heatCapAve << '\t' << MabsAve << '\t' << chiAbsAve << '\t'  << cumAve \
             << '\t' << aMabsAve << '\t' << achiAbsAve << '\t'  << acumAve<< '\n';
        file.close();    
    }
    
}

