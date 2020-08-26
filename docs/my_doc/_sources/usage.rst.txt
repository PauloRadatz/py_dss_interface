How to Use py-dss-interface Python package
==========================================

py-dss-interface Python package
-------------------------------

py-dss-interface is a Windows Python package providing access to OpenDSS
direct dll.

Simple Usage
------------

First import the Package

.. code:: ipython3

    import py_dss_interface

Creates an OpenDSS object

.. code:: ipython3

    dss = py_dss_interface.DSSDLL()


.. parsed-literal::

    OpenDSS Started successfully! 
    OpenDSS Version 8.6.7.1 (64-bit build); License Status: Open 
    
    
    

If you want to use your OpenDSS, you will need to pass the OpenDSS path
as argument to the DSSDLL class, as can be seen below:

.. code:: ipython3

    opendss_path = "C:/Program Files/OpenDSS"

Creates a dss object with your OpenDSS

.. code:: ipython3

    dss = py_dss_interface.DSSDLL(opendss_path)


.. parsed-literal::

    OpenDSS Started successfully! 
    OpenDSS Version 8.6.7.1 (64-bit build); License Status: Open 
    
    
    

Select the DSS model

.. code:: ipython3

    dss_file = "C:/MeuTCC/Paulo_Example/DSSFiles/MASTER_RedeTeste13Barras.dss"

Compile

.. code:: ipython3

    dss.text("compile {}".format(dss_file))

Solve - You can use the text interface as well: dss.text(“solve”)

.. code:: ipython3

    dss.solution_solve()




.. parsed-literal::

    0



Show Voltage Report

.. code:: ipython3

    dss.text("show voltages")

Get all buses voltages

.. code:: ipython3

    allbusvolts = dss.circuit_allbusvolts()

.. code:: ipython3

    print(dss.circuit_allbusvolts())


.. parsed-literal::

    (57499.9999610187, 33197.64035425945, -0.00013864574766527993, -66395.2808862234, -57499.99982237292, 33197.640544921145, 2521.7936567074075, -0.12510951454010835, -1245.9862419752644, -2157.929196146543, -1260.779296188911, 2184.0087426376244, 2458.7452442613235, -81.39453671847578, -1287.4206448519533, -2135.9298567530063, -1161.3323910051067, 2147.6615764261824, 2444.326824561451, -110.38241884322855, -1295.030311369754, -2146.289039370251, -1134.8987008510005, 2138.110201335036, 2412.4956463639573, -168.43605625253926, -1312.8440382618196, -2171.697240903866, -1094.0132554038855, 2121.5138394679016, 2412.464401702952, -168.48485198365645, -1312.8943145725611, -2171.6582918015097, -1093.956540127373, 2121.523662164901, 2452.1725646691275, -83.64244800590971, -1286.8199890553635, -2131.4802050651765, -1158.4948963219956, 2142.792422636375, -1160.2750765225585, 2143.428634119814, -1283.29380580493, -2115.248739030455, -1159.7097527534622, 2138.6939344252232, -1283.8968417967158, -2110.5129105938104, 2412.2522066242996, -168.58231335941676, -1312.94370402918, -2171.6858375682955, -1093.8042287021176, 2121.43566041516, 2398.2992799785643, -177.39256687081132, -1321.7044526217403, -2172.50036085359, -1092.234954270006, 2118.295407525877, 2408.1288090222597, -168.8789287612072, -1088.5730673552343, 2119.8261465648075, -1081.6701895532494, 2118.9162706766347, 2396.1883156224408, -165.2950715305662, 276.890815679254, -12.361421112745967, -147.8485332282431, -240.95700721018932, -129.60836606280253, 244.10255271131874, 2401.7765426227256, -0.0012353461198827344, -1200.8892314219984, -2079.9993794713546, -1200.8871219583307, 2080.000209819562)
    
