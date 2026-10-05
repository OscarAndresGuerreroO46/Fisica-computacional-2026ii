program ajuste_pendulo
  use iso_fortran_env, only: real64, iostat_end  ! Precisión y fin de archivo
  implicit none                                  ! Evita variables implícitas
  integer :: unidad, estado, n , unidad_salida                  ! Control del archivo y contador
  real(real64) :: x, y, sx, sy, sxx, sxy, den, a, b  ! Datos, sumas y parámetros
  real(real64) :: r2, g                      ! Parámetros calculados
  real(real64) :: sst, sse                       ! Variaciones y residuos
  real(real64) :: dato1,dato2,dato3,dato4,dato5  ! Datos de lectura
  real(real64) :: y_ajustada
  real(real64), parameter :: pi=3.14159265359_real64               ! Constante matemática
  ! Inicializa el contador y los acumuladores antes de leer
  n = 0
  sx = 0.0_real64
  sy = 0.0_real64
  sxx = 0.0_real64
  sxy = 0.0_real64

  ! Abre el archivo de cinco columnas generado anteriormente
  open(newunit=unidad, file='pendulo_limpio.dat', status='old', &
       action='read', iostat=estado)
  if (estado /= 0) error stop 'No fue posible abrir el archivo'

  ! Lee cada columna,define a x e y y construye las sumas necesarias
  do
    read(unidad, *, iostat=estado) dato1,dato2,dato3,dato4,dato5
    if (estado == iostat_end) exit
    if (estado /= 0) error stop 'Error de lectura'
    x=dato2/100.0_real64
    y=dato5/dato4
    y=y*y
    n = n + 1           ! Cuenta el par leído
    sx = sx + x         ! Acumula x
    sy = sy + y         ! Acumula y
    sxx = sxx + x*x     ! Acumula x^2
    sxy = sxy + x*y     ! Acumula x*y

  end do
  close(unidad)         ! Cierra el archivo después de la lectura

   if (n<2) error stop 'El archivo debe tener al menos dos datos'

  ! Usa las sumas para calcular la recta y mostrar sus parámetros
  den = real(n, real64)*sxx - sx*sx
  if (den == 0.0_real64) error stop 'El denominador es nulo'
  a = (real(n, real64)*sxy - sx*sy) / den
  b = (sy - a*sx) / real(n, real64)
  g = 4.0_real64*pi*pi/a

  ! Segunda pasada: calcula residuos, SSE y variación total SST
  sse = 0.0_real64
  sst = 0.0_real64
   open(newunit=unidad, file='pendulo_limpio.dat', status='old', &
       action='read', iostat=estado)
  if (estado /= 0) error stop 'No fue posible abrir el archivo'
  do
    read(unidad, *, iostat=estado) dato1,dato2,dato3,dato4,dato5
    if (estado == iostat_end) exit
    if (estado /= 0) error stop 'Error de lectura'
    x=dato2/100.0_real64
    y=dato5/dato4
    y=y*y
    y_ajustada = a*x + b                       ! Predicción de la recta
    sse = sse + (y - y_ajustada)**2           ! Suma de residuos al cuadrado
    sst = sst + (y - sy/real(n, real64))**2  ! Variación alrededor de la media
  end do
  close(unidad)
  if (sst == 0.0_real64) error stop 'La variación total es nula'
  r2 = 1.0_real64 - sse/sst
  open(newunit=unidad_salida, file='resultados_ajuste.dat', &
     status='replace', action='write')
write(unidad_salida, *) n, a, b, r2, g
close(unidad_salida)

  ! Muestra los resultados
  print *, 'a =', a, 'b =', b, 'r2 =', r2, 'g =', g

end program ajuste_pendulo
