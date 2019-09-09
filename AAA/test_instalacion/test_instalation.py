import os
import numpy as np
import keras as k
import tensorflow as tf

print("numpy=={}".format(np.__version__))
print("tensorflow=={}".format(tf.__version__))
print("keras: {}".format(k.__version__))

from keras.applications.inception_v3 import InceptionV3
from keras.preprocessing import image
from keras.applications.inception_v3 import preprocess_input, decode_predictions

# cargamos el modelo
model = InceptionV3(weights='imagenet')

img_path = 'elefante.jpg'
if not os.path.isfile(img_path):
    print("No existe el archivo {}".format(img_path))
    exit(-1)
    
img = image.load_img(img_path, target_size=(299, 299))
x = image.img_to_array(img)
x = np.expand_dims(x, axis=0)
x = preprocess_input(x)

preds = model.predict(x)

# decodificamos los resultados en una lista de tuplas (clase, descripcion, probabilidad)
# una lista para cada ejemplo en el batch
n=3
results = decode_predictions(preds, top=n)[0]
assert len(results) == n

for cls, dsc, p in results:
    print('{}: {} {:.3f}'.format(cls, dsc, p))
"""
Salida similar a:
n02504458: African_elephant 0.711
n01871265: tusker 0.208
n02504013: Indian_elephant 0.037
"""
