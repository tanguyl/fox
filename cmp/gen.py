import numpy as np
import timeit
N = 3000

def gen(N):
     X = np.array([1/(N-1),0,0])
     Y = np.array([0,1/(N-1),0])
     Z = np.array([0,0,1/4])
 
     W = np.array(np.linspace(0, N-1, N)).reshape((N, 1))
     H = np.array(np.linspace(N-1, 0, N)).reshape((N, 1, 1))
     return X*W + (Y*H+Z)


if __name__=="__main__":
    print(timeit.Timer(lambda: gen(N)).timeit(100) / 100.0)