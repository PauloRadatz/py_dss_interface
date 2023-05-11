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

    dss = py_dss_interface.DSS()


.. parsed-literal::

    OpenDSS Started successfully!
    OpenDSS Version 9.3.0.1 (64-bit build); License Status: Open




If you want to use your OpenDSS, you will need to pass the OpenDSS path
as argument to the DSS class, as can be seen below:

.. code:: ipython3

    opendss_path = "C:/Program Files/OpenDSS"

Creates a dss object with your OpenDSS

.. code:: ipython3

    dss = py_dss_interface.DSS(opendss_path)


.. parsed-literal::

    OpenDSS Started successfully!
    OpenDSS Version 9.2.0.1 (64-bit build); License Status: Open




Select the DSS model

.. code:: ipython3

    dss_file = "C:/MeuTCC/Paulo_Example/DSSFiles/MASTER_RedeTeste13Barras.dss"

Compile

.. code:: ipython3

    dss.text("compile {}".format(dss_file))




.. parsed-literal::

    ''



Solve - You can use the text interface as well: dss.text(“solve”)

.. code:: ipython3

    dss.solution.solve()




.. parsed-literal::

    0



Show Voltage Report

.. code:: ipython3

    dss.text("show voltages")




.. parsed-literal::

    ''



Get all buses voltages

.. code:: ipython3

    allbusvolts = dss.circuit_all_bus_volts()

.. code:: ipython3

    print(dss.circuit_all_bus_volts())


.. parsed-literal::

    [57499.9999611886, 33197.64035949601, -0.00013449617643016606, -66395.28088703856, -57499.99982669239, 33197.64054049976, 2521.7954836507547, -0.11910264415748555, -1245.983653171188, -2157.930119652756, -1260.782309001901, 2184.0080773974814, 2461.932156820345, -76.36035906329018, -1284.5460767592406, -2134.6137221915546, -1162.2387444138396, 2149.367176421046, 2447.622527822158, -105.33323048056116, -1292.0799754959983, -2145.0014944867958, -1135.8268535116601, 2139.8329412956255, 2416.0125196180225, -163.36016245424702, -1309.7330667771344, -2170.4745203525963, -1094.9726480270429, 2123.26458714393, 2416.0126942531097, -163.3602301351151, -1309.733210696227, -2170.474634870388, -1094.9726875020344, 2123.2647662400955, 2456.6075093447926, -77.22909111556126, -1283.252068702874, -2130.2151765650274, -1159.8081395939932, 2145.242676822373, -1161.1768654509938, 2145.138477965435, -1280.4300435279195, -2113.915974006277, -1160.6073109958113, 2140.403981134188, -1281.0373135073385, -2109.180381738326, 2415.769857096313, -163.5069028811886, -1309.832637909631, -2170.4632089820057, -1094.7636605550108, 2123.186733078666, 2401.861193733008, -172.353118727211, -1318.579879780884, -2171.2963607061065, -1093.1900162376958, 2120.0426886068794, 2411.6393197737566, -163.81660155967867, -1089.5319085584983, 2121.576761353844, -1082.6282761181412, 2120.6709717842764, 2399.6753950529396, -160.25426417938482, 278.27148663424725, -10.493493912035552, -146.96678765905787, -241.22429003185408, -130.42325689990824, 244.48803229938719, 206.94129996491046, -35.38505415088836, -142.73287289392556, -183.59574244679482, -91.6887699449892, 207.03653181237675, 2401.7765548121, -0.0011773940893547467, -1200.8892060353498, -2079.9993861623666, -1200.8871497819646, 2080.0002010072703, 278.77068751378476, -18.84925622382503, -151.12305792697495, -250.4393740486974, -126.34299926170385, 244.99208111611046]

