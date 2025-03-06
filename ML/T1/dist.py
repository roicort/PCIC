import scipy.stats as stats
import math

x = float(input("Introduce el valor de x: "))
mu = float(input("Introduce la media: "))
sigma = math.sqrt(float(input("Introduce la varianza: ")))
intervalo = float(input("Introce el intervalo de confianza:"))

x_inferior = x - intervalo
x_superior = x + intervalo

z_inferior = (x_inferior - mu) / sigma
z_superior = (x_superior - mu) / sigma

P_inferior = stats.norm.cdf(z_inferior)
P_superior = stats.norm.cdf(z_superior)

P = P_superior - P_inferior

print("P(%.2f < X < %.2f) = %.8f" % (x_inferior, x_superior, P))
