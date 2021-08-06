install.packages("keras")
install.packages("tensorflow")
#keras 를 설치하기 앞서 우선 anaconda를 다운로드 받아야 한다.
library(keras)
library(tensorflow)
library(reticulate) # 파이썬 연동 패키지

# create a new environment 
conda_create("r-reticulate") #처음 설치할때 conda환경에 따로 파일을 만들고 거기에 다운받아야 한다.
use_condaenv('r-reticulate') #환경을 만든 후 그 환경을 사용한다는 뜻
install_tensorflow(method = 'conda', envname = 'r-reticulate')
install_keras(method = 'conda', envname = 'r-reticulate')

#gpu 연동하는 방법도 따로 있음 제가 gpu가 없어서..

tf$constant("Hellow Tensorflow") #Check if tensorflow is active



#어제까지 잘됐는데 아침에 keras 라이브러리가 불러와지지않는 오류가 발생.
#이럴때는 anaconda prompt 창에 먼저 
#conda create -n env_name python=3.6 tensorflow
#후에 환경만들고 사용하기 해야 오류가 안생긴다.
