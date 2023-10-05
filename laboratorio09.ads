package Laboratorio09 is
   ------------------------------------------------------------
   -- TAREA 1
   ------------------------------------------------------------
   Max_Valores: constant integer := 20;
   type T_Vector_Enteros is array (Integer range <>) of Integer;
   type T_Lista_Estatica is record
      Dato: T_Vector_Enteros(1..Max_Valores);
      cont: Natural;
   end record;

   procedure Insertar_En_Medio(L: in out T_Lista_Estatica; Num: in Integer);
   -- post: Num se inserta en el medio de L.

   procedure Borrar_Intermedio(L: in out T_Lista_Estatica);
   -- post: L es la de partida sin el elemento intermedio

   procedure Encriptar(N,Clave: in T_Vector_Enteros; Num: out T_Vector_Enteros);
   -- pre: N es un vector de dígitos (entre 0..9)
   --      Clave es un vector con numeros de 1 a tamaño de N
   -- post: Num contiene el numero con los digitos de N usando Clave

   function Comunes(V1,V2: in T_vector_enteros) return Natural;
   -- pre: V1 y V2 ordenados ascendentemente
   -- post: resultado = numero de elementos comunes de V1 y V2

   Max: constant Natural := 3546;
   type T_Vecino is record
      nombre: String(1..25);
      piso: integer range 1..100;
      mano: character range 'A'..'J';
   end record;
   type T_Comunidad is array (1..Max) of T_Vecino;
   type T_Rascacielos is array (Integer range 1..100, Character range 'A'..'J') of Natural;

   procedure Obtener_Num_Vecinos_por_Vivienda (C: in T_Comunidad;R:out T_Rascacielos);
   --pre: C contiene los datos de los vecinos de la comunidad
   --post: R contiene los datos de cuántos vecinos hay por cada vivienda según lo que aparece en C

   type T_Vivienda is record
      Consumo_Electrico, Consumo_Gas: Float;
      Habitantes: Integer;
   end record;

   type T_Edificio is array (Character range 'A'..'J', Integer range 1..100) of T_Vivienda;
   type T_Consumo_Medio_Manos is array (Character range 'A'..'J') of Float;

   procedure Obtener_Consumos (E: in T_Edificio;
      Consumo_electrico: out Float;
      Consumo_Gas: out T_Consumo_Medio_Manos);

   subtype T_Dia is Integer range 1..31;
   type T_Mes is (Enero, Febrero, Marzo, Abril, Mayo, Junio, Julio, Agosto, Septiembre, Octubre, Noviembre, Diciembre);
   type T_Ano_Lluvias is array(T_Mes) of T_Vector_enteros(1..31);
   type T_Datos_Pluviometricos is array(2001..2020) of T_Ano_Lluvias;
   type T_Fecha is record
      Dia: T_Dia;
      Mes: T_Mes;
      anno: Positive;
   end record;
   procedure Maximo(Lluvia: in T_Datos_Pluviometricos; F1, F2: T_Fecha; F: out T_Fecha);
   --pre: F1 y F2 dos fechas y F1 anterior a F2
   --post: Mes y ano contienen el mes y el año que mas llovio entre esas fechas
   --      contando solo los días incluidos de cada mes


   ------------------------------------------------------------
   -- TAREA 2
   ------------------------------------------------------------
      --Lista estática de carreteras
   Max_Car: constant Integer := 110;
   Max_Carreteras: constant Integer := 40;
   Max_Puntos: constant Integer:=1000;
   type T_Punto is record
      X, Y: Float;
   end record;
   CENTINELA: Constant T_Punto := (-1.0, -1.0);
   type V_Puntos is array (Integer range <>)of T_Punto;
   type T_Estatica_Puntos is record
      Ptos: V_Puntos(1..Max_puntos);
      Num: Natural;
   end record;
   type T_Carretera is record
      Codigo, Inicio, Final: String(1..Max_Car);
      Peaje_km: Float;
      Trazado: T_Estatica_Puntos;
   end record;
   type V_Carreteras is array (Integer range <>) of T_Carretera;
   type T_Estatica_Carreteras is record
      Ctras: V_Carreteras(1..Max_Carreteras);
      Cont: Natural;
   end record;

   procedure Simplificar(L:  in out T_Estatica_Carreteras;
                         Peaje: out T_Estatica_Carreteras);
   --pre: Lista de carreteras
   --post: Peaje = cinco primeras carreteras de peaje de L (o menos si no hay 5)
   --      L = vector original sin las cinco primeras carreteras de peaje (o las que haya)
   procedure Simplificar(L: in out T_Estatica_Puntos);
   --pre: Lista de puntos ordenada
   --post: Misma lista de puntos eliminando 3 de cada cuatro conservando los extremos
   --      cuantos indica cuantos puntos quedan en la lista.
   procedure Simplificar(L: in out T_Estatica_Carreteras);
   --pre: Lista de carreteras con muchos puntos
   --post: Lista de carreteras con los puntos cercanos eliminados

   ------------------------------------------------------------
   -- TAREA 3
   ------------------------------------------------------------
   type T_Identificacion is record
      Nom, Apel: String (1 .. 20);
      Dni: Integer;
   end record;
   type T_Info_Apartamento is record
      Precio : Integer;
      Propietario_Actual: T_Identificacion;
      Propietario_Anterior: T_Identificacion;
   end record;
   Max_Apartamentos_Por_Planta: constant Integer:= 8;
   type T_Tabla_Apartamentos is array (Integer range <>) of T_Info_Apartamento;
   type T_Planta is record
      Apartamentos: T_Tabla_Apartamentos(1 .. Max_Apartamentos_Por_Planta);
      Num_Apartamentos: Integer; -- valor entre 1 y 8
   end record;
   Max_Plantas : constant Integer := 10;
   type T_Edificacion is array (Integer range <>) of T_Planta;
   Num_Bloques : constant Integer := 20;
   type T_Urbanizacion is array (1 .. Num_Bloques) of T_Edificacion(1 .. Max_Plantas);
   function DNI_Mayor_Patrimonio(U: in T_Urbanizacion) return Integer;
   -- pre:
   -- post: resultado = el DNI de la persona de la urbanización que tiene
   -- un mayor patrimonio (el patrimonio es la suma de los valores
   -- de los pisos que posee)

   N: constant integer:=9;
   M: constant integer:=3;
   VACIO: constant integer := 0;
   type T_Puzle is array (1..N,1..N) of integer;
   type T_Ficha is array (1..M, 1..M) of natural;
   procedure encontrar_espacio_blanco(P: in T_Puzle; F, C: out Integer);
   --pre: P tiene un espacio en blanco para una única ficha.
   --post: 1<=F,C<=9. F y C son las coordenadas de la esquina superior izquierda del sudoku donde se encuentra su espacio en blanco
   procedure rotar_matriz_derecha_90 (F: in out T_Ficha);
   --post: F contiene la ficha de la entrada rotada 90º a la derecha
   function filas_correctas (P: in T_Puzle; Ficha: in T_Ficha; F, C: in Integer) return Boolean;
   --Pre: Los números ya colocados en el sudoku son correctos. La ficha está aún sin colocar. El sudoku tiene un único espacio en blanco para una ficha
   --post: El resultado es true sii las filas de P cumplen los requisitos de las filas una vez añadida la ficha F en el espacio cuya esquina superior izquierda es la fila F y la columna C (y 2 siguientes).

end Laboratorio09;

