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


If you want to use your OpenDSS, you will need to pass the OpenDSS path as argument to the DSSDLL class, as can be seen below:

.. code:: ipython3

    opendss_path = r"C:\Program Files\OpenDSS"

Creates a dss object with your OpenDSS

.. code:: ipython3

    dss = py_dss_interface.DSSDLL(opendss_path)

.. parsed-literal::

    OpenDSS Started successfully!
    OpenDSS Version 8.6.7.1 (64-bit build); License Status: Open

Select the DSS model

.. code:: ipython3

    dss_file = r"C:\MeuTCC\Paulo_Example\DSSFiles\MASTER_RedeTeste13Barras.dss"

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

    (57502.68657444459, 33189.47582751112, -10.988164394741505, -66394.86913593691, -57491.69841004984, 33205.3933213843, 2401.5628101023703, -0.4668918200941385, -1201.237671677943, -2079.7175222940314, -1200.311653100388, 2080.141949991376, 2536.3561735751905, -0.5793185283079451, -1246.2598694166902, -2157.48772341857, -1267.5878413546532, 2196.935571488354, 2426.425943474452, -109.95862245792503, -1300.0214661176267, -2096.2770897808796, -1120.4367179288215, 2128.614974757371, 273.12114108033103, -15.65235754302226, -149.2212490974322, -236.28695774995873, -124.74067151469185, 242.00907089788834, 2350.078612278651, -221.06904907887537, -1338.4093354936983, -2109.7885897297024, -1015.4271681140748, 2083.131981642726, -1295.6881026261967, -2078.358923233928, -1122.400505964402, 2129.5714566690303, -1296.245262941303, -2073.152331293206, -1121.8021030579102, 2124.3630783208437, -1015.4271623240293, 2083.1319645675635, 2350.0785903696974, -221.06904177805635, -1338.4093393325584, -2109.7885840555614, 2333.5010209870284, -229.74415641869274, -1347.9866729361388, -2110.40099772813, -1013.9884003655168, 2078.6662098826223, -1002.146986487873, 2078.7704807720934, 2332.4283404892512, -217.29865419693618, 2407.0547918738666, -145.36409129295672, -1312.3049779525563, -2102.363108766874, -1083.0150310009187, 2116.119667082487, 2433.849895854283, -107.51619491330662, -1300.765241902797, -2101.2624606223394, -1123.5766850756647, 2134.1483915378726, 2350.0786408803283, -221.06906658917558, -1338.4093587456503, -2109.788617194392, -1015.4271724421402, 2083.1320163774312, 2345.3894199668225, -221.58915837414344, -1009.5895426635545, 2080.54927101747)

