#include <iostream>

using namespace std;


int cell(int x, int y, int puzzle) {
  int rackID = x + 10;
  int powerLevel = (((rackID * y + puzzle) * rackID) % 1000) / 100;

  return (powerLevel - 5);
}

int main() {
  int puzzle = 9435;

  int data[300][300];
  int integral[300][300];

  for(int x=0; x<300; x++) {
    for(int y=0; y<300; y++) {
      data[x][y] = cell(x+1, y+1, puzzle);
    }
  }

  // integral
  integral[0][0] = data[0][0];

  for(int x=1; x<300; x++) {
    integral[x][0] = integral[x-1][0] + data[x][0];
  }

  for(int y=1; y<300; y++) {
    integral[0][y] = integral[0][y-1] + data[0][y];
  }

  for(int x=1; x<300; x++) {
    for(int y=1; y<300; y++) {
      integral[x][y] = data[x][y] + integral[x][y-1] + integral[x-1][y] - integral[x-1][y-1];
    }
  }

  int max = integral[1][1];
  int max_x = 0;
  int max_y = 0;
  int max_n = 1;

  for(int n=2; n<300; n++) {
    int root = integral[n-1][n-1];
    if (root > max) {
        max = root;
        max_x = 0;
        max_y = 0;
        max_n = n;
    }

    for(int x=1; x<300-n; x++) {
      int value = integral[x-1+n][n-1] - integral[x-1][n-1];

      if (value > max) {
          max = value;
          max_x = x;
          max_y = 0;
          max_n = n;
      }
    }

    for(int y=1; y<300-n; y++) {
      int value = integral[n-1][y-1+n] - integral[n-1][y-1];

      if (value > max) {
          max = value;
          max_x = 0;
          max_y = y;
          max_n = n;
      }
    }

    for(int x=1; x<300-n; x++) {
      for(int y=1; y<300-n; y++) {
        int value = integral[x-1+n][y-1+n] + integral[x-1][y-1] - integral[x-1+n][y-1] - integral[x-1][y-1+n];

        if (value > max) {
            max = value;
            max_x = x;
            max_y = y;
            max_n = n;
        }
      }
    }
  }

  std::cout << max << " " << max_x+1 << " " << max_y+1 << " " << max_n << std::endl;

//   std::cout << data[216][195] << std::endl;
// 90,269
  // calculations
  // for(int x=0; x<300; x++) {
  //   for(int y=0; y<300; y++) {
  //     std::cout << integral[x][y] << " ";
  //   }
  //   std::cout << std::endl;
  // }


  return 0;
}
