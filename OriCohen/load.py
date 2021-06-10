from sklearn.datasets import fetch_openml


trainset_size = 60000 # ie., testset_size = 10000

def download():
 mnist = fetch_openml('MNIST original')
 X = mnist.data.astype('float64')
 y = mnist.target
 print ('MNIST:', X.shape, y.shape)
 return (X, y)

def split(train_size):
 X_train_full = X[:train_size]
 y_train_full = y[:train_size]
 X_test = X[train_size:]
 y_test = y[train_size:]
 return (X_train_full, y_train_full, X_test, y_test)