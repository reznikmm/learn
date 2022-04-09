Стандартная библиотека: Numerics
================================

.. include:: ../../global.txt

Стандартная библиотека обеспечивает поддержку общих числовых операций
для типов с плавающей запятой, а также для сложных типов и матриц. В
нижеследующих разделах приводится краткое введение в эти числовые
операции.

Элементарные функции
--------------------

Пакет :ada:`Ada.Numerics.Elementary_Functions` обеспечивает общие операции
для типов плавающих точек, таких как квадратный корень, логарифм и
тригонометрические функции (например, sin, cos). Например:

.. code-block:: ada

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Numerics; use Ada.Numerics;

    with Ada.Numerics.Elementary_Functions;
    use  Ada.Numerics.Elementary_Functions;

    procedure Show_Elem_Math is
       X : Float;
    begin
       X := 2.0;
       Put_Line ("Square root of "
                 & Float'Image (X)
                 & " is "
                 & Float'Image (Sqrt (X)));

       X := e;
       Put_Line ("Natural log of "
                 & Float'Image (X)
                 & " is "
                 & Float'Image (Log (X)));

       X := 10.0 ** 6.0;
       Put_Line ("Log_10      of "
                 & Float'Image (X)
                 & " is "
                 & Float'Image (Log (X, 10.0)));

       X := 2.0 ** 8.0;
       Put_Line ("Log_2       of "
                 & Float'Image (X)
                 & " is "
                 & Float'Image (Log (X, 2.0)));

       X := Pi;
       Put_Line ("Cos         of "
                 & Float'Image (X)
                 & " is "
                 & Float'Image (Cos (X)));

       X := -1.0;
       Put_Line ("Arccos      of "
                 & Float'Image (X)
                 & " is "
                 & Float'Image (Arccos (X)));
    end Show_Elem_Math;

Здесь мы используем стандартные константы :ada:`e` и :ada:`Pi` из пакета :ada:`Ada.Numerics`.

Пакет :ada:`Ada.Numerics.Elementary_Functions` предоставляет операции для типа :ada:`Float`. Подобные пакеты доступны для
типов :ada:`Long_Float` и :ada:`Long_Long_Float`. Например, пакет :ada:`Ada.Numerics.Long_Elementary_Functions` предлагает тот же набор операций для типа :ada:`Long_Float`
.Кроме того, пакет :ada:`Ada.Numerics.Generic_Elementary_Functions` ‑ это универсальная версия пакета, которую можно
создать для пользовательских типов с плавающей запятой. Фактически,
пакет :ada:`Elementary_Functions` можно определить следующим образом:

.. code-block:: ada

    package Elementary_Functions is new
      Ada.Numerics.Generic_Elementary_Functions (Float);

Генерация случайных чисел
-------------------------

Пакет :ada:`Ada.Numerics.Float_Random` предоставляет простой генератор случайных чисел для диапазона
от 0,0 до 1,0. Чтобы использовать его, объявите генератор :ada:`G`, который вы
передадите в :ada:`Random`. Например:

.. code-block:: ada

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

    procedure Show_Float_Random_Num is
       G : Generator;
       X : Uniformly_Distributed;
    begin
       Reset (G);

       Put_Line ("Some random numbers between "
                 & Float'Image (Uniformly_Distributed'First)
                 & " and "
                 & Float'Image (Uniformly_Distributed'Last)
                 & ":");
       for I in 1 .. 15 loop
          X := Random (G);
          Put_Line (Float'Image (X));
       end loop;
    end Show_Float_Random_Num;

Стандартная библиотека также включает генератор случайных чисел для
дискретных чисел, который является частью пакета :ada:`Ada.Numerics.Discrete_Random`. Поскольку это
универсальный пакет, необходимо создать его экземпляр для требуемого
дискретного типа. Это позволяет задать диапазон для генератора. В
следующем примере создается приложение, отображающее случайные целые
числа от 1 до 10:

.. code-block:: ada

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Numerics.Discrete_Random;

    procedure Show_Discrete_Random_Num is

       subtype Random_Range is Integer range 1 .. 10;

       package R is new
         Ada.Numerics.Discrete_Random (Random_Range);
       use R;

       G : Generator;
       X : Random_Range;
    begin
       Reset (G);

       Put_Line ("Some random numbers between "
                 & Integer'Image (Random_Range'First)
                 & " and "
                 & Integer'Image (Random_Range'Last)
                 & ":");

       for I in 1 .. 15 loop
          X := Random (G);
          Put_Line (Integer'Image (X));
       end loop;
    end Show_Discrete_Random_Num;

Здесь пакет :ada:`R` создается с типом :ada:`Random_Range`, который имеет ограниченный диапазон
от 1 до 10. Это позволяет нам контролировать диапазон, используемый
для случайных чисел. Мы могли бы легко изменить приложение для
отображения случайных целых чисел от 0 до 20, изменив спецификацию
типа :ada:`Random_Range`. Мы также можем использовать типы с плавающей запятой или с
фиксированной запятой.

Тип комплексного числа
----------------------

Пакет :ada:`Ada.Numerics.Complex_Types` обеспечивает поддержку типов комплексных чисел, а пакет :ada:`Ada.Numerics.Complex_Elementary_Functions`
обеспечивает поддержку общих операций с типами комплексных чисел,
аналогично пакету :ada:`Ada.Numerics.Elementary_Functions`. Наконец, вы можете использовать пакет :ada:`Ada.Text_IO.Complex_IO` для
выполнения операций ввода-вывода над комплексными числами. В следующем
примере мы объявляем переменные типа :ada:`Complex` и инициализируем их с помощью
агрегата:

.. code-block:: ada

    with Ada.Text_IO;  use Ada.Text_IO;
    with Ada.Numerics; use Ada.Numerics;

    with Ada.Numerics.Complex_Types;
    use  Ada.Numerics.Complex_Types;

    with Ada.Numerics.Complex_Elementary_Functions;
    use  Ada.Numerics.Complex_Elementary_Functions;

    with Ada.Text_IO.Complex_IO;

    procedure Show_Elem_Math is

       package C_IO is new
         Ada.Text_IO.Complex_IO (Complex_Types);
       use C_IO;

       X, Y  : Complex;
       R, Th : Float;
    begin
       X := (2.0, -1.0);
       Y := (3.0,  4.0);

       Put (X);
       Put (" * ");
       Put (Y);
       Put (" is ");
       Put (X * Y);
       New_Line;
       New_Line;

       R  := 3.0;
       Th := Pi / 2.0;
       X  := Compose_From_Polar (R, Th);
       --  Alternatively:
       --  X := R * Exp ((0.0, Th));
       --  X := R * e ** Complex'(0.0, Th);

       Put ("Polar form:    "
            & Float'Image (R)  & " * e**(i * "
            & Float'Image (Th) & ")");
       New_Line;

       Put ("Modulus     of ");
       Put (X);
       Put (" is ");
       Put (Float'Image (abs (X)));
       New_Line;

       Put ("Argument    of ");
       Put (X);
       Put (" is ");
       Put (Float'Image (Argument (X)));
       New_Line;
       New_Line;

       Put ("Sqrt        of ");
       Put (X);
       Put (" is ");
       Put (Sqrt (X));
       New_Line;
    end Show_Elem_Math;

Как видно из этого примера, все общие операторы, такие, как :ada:`*` и :ada:`+`,
доступны для сложных типов. У вас также есть типичные операции с
комплексными числами, такие как :ada:`Argument` и :ada:`Exp`. Помимо инициализации комплексных
чисел в декартовой форме с использованием агрегатов, вы можете сделать
это из полярной формы, вызвав функцию :ada:`Compose_From_Polar`.

Пакеты :ada:`Ada.Numerics.Complex_Types` и :ada:`Ada.Numerics.Complex_Elementary_Functions` предоставляют операции для типа :ada:`Float`. Подобные пакеты доступны
для типов :ada:`Long_Float` и :ada:`Long_Long_Float`. Кроме того, пакеты :ada:`Ada.Numerics.Generic_Complex_Types` и :ada:`Ada.Numerics.Generic_Complex_Elementary_Functions` являются общими версиями,
которые можно создавать для пользовательских или предварительно
определенных типов с плавающей запятой. Например:

.. code-block:: ada

    with Ada.Numerics.Generic_Complex_Types;
    with Ada.Numerics.Generic_Complex_Elementary_Functions;
    with Ada.Text_IO.Complex_IO;

    procedure Show_Elem_Math is

       package Complex_Types is new
         Ada.Numerics.Generic_Complex_Types (Float);
       use Complex_Types;

       package Elementary_Functions is new
         Ada.Numerics.Generic_Complex_Elementary_Functions
           (Complex_Types);
       use Elementary_Functions;

       package C_IO is new Ada.Text_IO.Complex_IO
         (Complex_Types);
       use C_IO;

       X, Y  : Complex;
       R, Th : Float;

Манипулирование векторами и матрицами
-------------------------------------

Пакет :ada:`Ada.Numerics.Real_Arrays` обеспечивает поддержку векторов и матриц. Он включает в себя
общие операции с матрицами, такие как обратные, определитель,
собственные значения, в дополнение к более простым операторам, таким
как сложение и умножение матриц. Вы можете объявлять векторы и
матрицы, используя типы :ada:`Real_Vector` и :ada:`Real_Matrix` соответственно.

В следующем примере используются некоторые операции из пакета :ada:`Ada.Numerics.Real_Arrays`:

.. code-block:: ada

    with Ada.Text_IO;  use Ada.Text_IO;

    with Ada.Numerics.Real_Arrays;
    use  Ada.Numerics.Real_Arrays;

    procedure Show_Matrix is

       procedure Put_Vector (V : Real_Vector) is
       begin
          Put ("    (");
          for I in V'Range loop
             Put (Float'Image (V (I)) & " ");
          end loop;
          Put_Line (")");
       end Put_Vector;

       procedure Put_Matrix (M : Real_Matrix) is
       begin
          for I in M'Range (1) loop
             Put ("    (");
             for J in M'Range (2) loop
                Put (Float'Image (M (I, J)) & " ");
             end loop;
             Put_Line (")");
          end loop;
       end Put_Matrix;

       V1       : Real_Vector := (1.0, 3.0);
       V2       : Real_Vector := (75.0, 11.0);

       M1       : Real_Matrix :=
                    ((1.0, 5.0, 1.0),
                     (2.0, 2.0, 1.0));
       M2       : Real_Matrix :=
                    ((31.0, 11.0, 10.0),
                     (34.0, 16.0, 11.0),
                     (32.0, 12.0, 10.0),
                     (31.0, 13.0, 10.0));
       M3       : Real_Matrix := ((1.0, 2.0),
                                  (2.0, 3.0));
    begin
       Put_Line ("V1");
       Put_Vector (V1);
       Put_Line ("V2");
       Put_Vector (V2);
       Put_Line ("V1 * V2 =");
       Put_Line ("    "
                 & Float'Image (V1 * V2));
       Put_Line ("V1 * V2 =");
       Put_Matrix (V1 * V2);
       New_Line;

       Put_Line ("M1");
       Put_Matrix (M1);
       Put_Line ("M2");
       Put_Matrix (M2);
       Put_Line ("M2 * Transpose(M1) =");
       Put_Matrix (M2 * Transpose (M1));
       New_Line;

       Put_Line ("M3");
       Put_Matrix (M3);
       Put_Line ("Inverse (M3) =");
       Put_Matrix (Inverse (M3));
       Put_Line ("abs Inverse (M3) =");
       Put_Matrix (abs Inverse (M3));
       Put_Line ("Determinant (M3) =");
       Put_Line ("    "
                 & Float'Image (Determinant (M3)));
       Put_Line ("Solve (M3, V1) =");
       Put_Vector (Solve (M3, V1));
       Put_Line ("Eigenvalues (M3) =");
       Put_Vector (Eigenvalues (M3));
       New_Line;
    end Show_Matrix;

Размеры матрицы автоматически определяются на основе агрегата,
используемого для инициализации, если вы их не указываете. Однако вы
также можете использовать явные диапазоны. Например:

.. code-block:: ada

    M1       : Real_Matrix (1 .. 2, 1 .. 3) :=
                 ((1.0, 5.0, 1.0),
                  (2.0, 2.0, 1.0));

Пакет :ada:`Ada.Numerics.Real_Arrays` реализует операции для типа :ada:`Float`. Аналогичные пакеты доступны для
типов :ada:`Long_Float` и :ada:`Long_Long_Float`. Кроме того, пакет :ada:`Ada.Numerics.Generic_Real_Arrays` ‑ это универсальная версия, которую
можно создать с помощью пользовательских типов с плавающей запятой.
Например, пакет :ada:`Real_Arrays` может быть определен следующим образом:

.. code-block:: ada

    package Real_Arrays is new
      Ada.Numerics.Generic_Real_Arrays (Float);


