Записи
======

.. include:: ../../global.txt

Пока что все типы, с которыми мы столкнулись, имеют значения, которые
нельзя разложить: каждый экземпляр представляет собой неделимый
элемент данных. Теперь мы обратим наше внимание на наш первый
составной тип: записи.

Записи позволяют составлять значения из значений других типов.
Каждому из таких значений будет присвоено имя. Пара, состоящая из
имени и значения определенного типа, называется полем или
компонентой.

.. _Intro_Ada_Record_Type_Declaration:

Объявление типа записи
----------------------

Вот пример простого объявления записи:

.. code-block:: ada

    type Date is record
       --  The following declarations are
       --  components of the record
       Day   : Integer range 1 .. 31;
       Month : Months;
       --  You can add custom constraints
       --  on fields
       Year  : Integer range 1 .. 3000;
    end record;

Поля во многом похожи на объявления переменных, за исключением того,
что они находятся внутри определения записи. Как и при объявлениях
переменных, можно указать дополнительные ограничения при
предоставлении подтипа поля.

.. code-block:: ada

    type Date is record
       Day   : Integer range 1 .. 31;
       Month : Months := January;
       --  This component has a default value
       Year  : Integer range 1 .. 3000 := 2012;
       --                                 ^ Default value
    end record;

Компоненты записи могут иметь значения по умолчанию. Когда объявлена
переменная с типом записи, для поля с инициализацией
получат заданные значения автоматически.
Значение может быть задано любым выражением соответствующего типа и
может вычислятся во время исполнения.

Агрегаты
--------

.. code-block:: ada

    Ada_Birthday    : Date := (10, December, 1815);
    Leap_Day_2020   : Date := (Day   => 29,
                               Month => February,
                               Year  => 2020);
    --                         ^ By name

Записи имеют удобную форму для записи значений, показанную выше.
Эта нотация называется агрегированной, а сама конструкция -
агрегатом. Ее можно использовать в различных контекстах, которые мы
увидим на протяжении всего курса, и одним из применений является
инициализация записей.

Агрегат - это список значений, разделенных запятыми и заключенных в
круглые скобки. Он разрешен в любом контексте, где ожидается
значение записи.

Значения для компонент можно указать позиционно, как в примере
:ada:`Ada_Birthday`, или по имени, как в :ada:`Leap_Day_2020`.
Разрешено сочетание позиционных и именованных значений, но вы не можете
использовать позиционную форму после появления именованной.

Извлечение компонент
--------------------

Для доступа к компонентам экземпляра записи используется операция,
называемая извлечением компоненты. Она имеет форму точечной
нотации. Например, если мы объявляем переменную :ada:`Some_Day` типа
записи :ada:`Date`, упомянутого выше, мы можем получить доступ к компоненте
:ada:`Year`, написав :ada:`Some_Day.Year`.

Рассмотрим пример:

.. code:: ada run_button project=Courses.Intro_To_Ada.Records.Record_Selection

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Record_Selection is

       type Months is
         (January, February, March, April,
          May, June, July, August, September,
          October, November, December);

       type Date is record
          Day   : Integer range 1 .. 31;
          Month : Months;
          Year  : Integer range 1 .. 3000 := 2032;
       end record;

       procedure Display_Date (D : Date) is
       begin
          Put_Line ("Day:" & Integer'Image (D.Day)
                    & ", Month: "
                    & Months'Image (D.Month)
                    & ", Year:"
                    & Integer'Image (D.Year));
       end Display_Date;

       Some_Day : Date := (1, January, 2000);

    begin
       Display_Date (Some_Day);

       Put_Line ("Changing year...");
       Some_Day.Year := 2001;

       Display_Date (Some_Day);
    end Record_Selection;

Как вы можете видеть в этом примере, мы можем использовать точечную
нотацию в выражении :ada:`D.Year` или :ada:`Some_Day.Year`. как для
доступа к информации компонеты, так и для ее изменения в операторе
присваивания.
Говоря конкретнее, когда мы используем :ada:`D.Year` в вызове :ada:`Put_Line`,
мы читаем информацию, хранящуюся в этом компоненте. Когда мы пишем
:ada:`Some_Day.Year := 2001`, то перезаписываем информацию, которая
была ранее сохранена в компоненте :ada:`Year` в переменной :ada:`Some_Day`.

.. _Record_Comp_Renaming:

Переименование
--------------

В предыдущих главах мы обсуждали переименование
:ref:`подпрограмм <Subprogram_Renaming>` и :ref:`пакетов <Package_Renaming>`.
Мы также можем переименовывать компоненты записи. Вместо того, чтобы
каждый раз писать извлечение компоненты с использованием точечной записи, мы
можем объявить псевдоним, который позволит нам получить доступ к этой
компоненте. Это полезно, например, для упрощения реализации подпрограммы.

Мы можем переименовать компоненты записи, используя ключевое слово :ada:`renames` в
объявлении переменной. Например:

.. code-block:: ada

    Some_Day : Date;
    Y        : Integer renames Some_Day.Year;

Здесь :ada:`Y` является псевдонимом, так что каждый раз, когда мы используем :ada:`Y`,
мы в действительности используем компоненту :ada:`Year` переменной :ada:`Some_Day`.

Давайте рассмотрим полный пример:

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Record_Component_Renaming

    package Dates is

       type Months is
         (January, February, March, April,
          May, June, July, August, September,
          October, November, December);

       type Date is record
          Day   : Integer range 1 .. 31;
          Month : Months;
          Year  : Integer range 1 .. 3000 := 2032;
       end record;

       procedure Increase_Month (Some_Day : in out Date);

       procedure Display_Month (Some_Day : Date);

    end Dates;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Dates is

       procedure Increase_Month (Some_Day : in out Date) is
          --  Renaming components from
          --  the Date record
          M : Months  renames Some_Day.Month;
          Y : Integer renames Some_Day.Year;

          --  Renaming function (for Months
          --  enumeration)
          function Next (M : Months) return Months
            renames Months'Succ;
       begin
          if M = December then
             M := January;
             Y := Y + 1;
          else
             M := Next (M);
          end if;
       end Increase_Month;

       procedure Display_Month (Some_Day : Date) is
          --  Renaming components from
          --  the Date record
          M : Months  renames Some_Day.Month;
          Y : Integer renames Some_Day.Year;
       begin
          Put_Line ("Month: "
                    & Months'Image (M)
                    & ", Year:"
                    & Integer'Image (Y));
       end Display_Month;

    end Dates;

    with Ada.Text_IO; use Ada.Text_IO;
    with Dates;       use Dates;

    procedure Main is
       D : Date := (1, January, 2000);
    begin
       Display_Month (D);

       Put_Line ("Increasing month...");
       Increase_Month (D);

       Display_Month (D);
    end Main;

Мы применяем переименование к двум компонентам записи :ada:`Date` в реализации
процедуры :ada:`Increase_Month`. Затем вместо непосредственного использования
:ada:`Some_Day.Month` и :ada:`Some_Day.Year` в последующих
операциях мы просто используем переименованные версии :ada:`M` и :ada:`Y`.

Обратите внимание, что в приведенном выше примере мы также
переименовали :ada:`Months'Succ` - функцию, которая дает нам следующий
месяц - в :ada:`Next`.
