## Instalación keras en R

1. Instalar [Anaconda 3.7](https://www.anaconda.com/distribution/#download-section).
   Está disponible para Windows, Linux y MacOS
2. Instalar el paquete `keras` en R con `install.packages(keras)`
3. Cargar el paquete y ejecutar:
   ```
   library(keras)
   install_keras()
   ```
4. Si el paso anterior **NO** da error, hemos terminado. Probar a ejecutar
   alguno de los ejercicios de clase. Si da error, ya sea al instalar keras o al
   ejecutar alguno de los ejemplos, pasar al punto 5.
5. Abrir un prompt de Anaconda. En Linux, simplemente abrimos una terminal. En
   Windows, `Inicio > Anaconda3 > Anaconda Prompt`
6. Crear un nuevo entorno de conda con `conda create --name r-tensorflow`
7. Activar el entorno `conda activate r-tensorflow`
8. Instalar tensorflow y keras
   ```
   pip install tensorflow
   pip install keras
   ```
9. Volver a R y escribir:
   ```
   library(keras)
   use_condaenv('r-tensorflow')
   ```
10. Importante **NO** escribir `install_keras()` en R. Si todo ha ido bien,
    debería de estar instalado correctamente. Comprobar ejecutando un ejercicio
    de clase
