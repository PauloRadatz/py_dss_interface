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
    OpenDSS Version 9.1.0.1 (64-bit build); License Status: Open 
    
    
    

If you want to use your OpenDSS, you will need to pass the OpenDSS path
as argument to the DSSDLL class, as can be seen below:

.. code:: ipython3

    opendss_path = "C:/Program Files/OpenDSS"

Creates a dss object with your OpenDSS

.. code:: ipython3

    dss = py_dss_interface.DSSDLL(opendss_path)


.. parsed-literal::

    OpenDSS Started successfully! 
    OpenDSS Version 9.1.0.1 (64-bit build); License Status: Open 
    
    
    

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

    (57499.999961009074, 33197.640354273106, -0.00013863762045166066, -66395.28088623453, -57499.999822371436, 33197.6405449186, 2521.7936573110283, -0.12510639684095803, -1245.9862262218212, -2157.929209035358, -1260.7792931316696, 2184.0087426899695, 2458.754690794299, -81.40012844432013, -1287.4048041634844, -2135.946271741362, -1161.3237710633591, 2147.655221970978, 2444.339467988154, -110.38987659349553, -1295.0090374772096, -2146.3108798904473, -1134.8871913334083, 2138.101758577822, 2412.514691359986, -168.4472092940003, -1312.8118448431376, -2171.729866036818, -1093.995996680623, 2121.501290386275, 2412.488005942499, -168.49866507278261, -1312.8540868006478, -2171.698661190646, -1093.9352855713466, 2121.5084668051377, 2452.182028400439, -83.64802338349054, -1286.8042029857997, -2131.4966219461653, -1158.486273837127, 2142.7860464679807, -1160.2664734251896, 2143.4222555432316, -1283.2781065186139, -2115.26512230729, -1159.7011401523832, 2138.6875459419207, -1283.8811521026157, -2110.5292840271613, 2412.271253656012, -168.593465672597, -1312.9115112528523, -2171.7184643942674, -1093.7869695191473, 2121.423108635722, 2398.3183957401716, -177.40376853118553, -1321.6723142417534, -2172.5331372609717, -1092.217709971331, 2118.2826501937852, 2408.1478021636167, -168.89008046228318, -1088.5558383806967, 2119.813535485816, -1081.6529752146193, 2118.9035980002122, 2396.2072257326504, -165.3061454935626, 276.8919268866152, -12.362040877727702, -147.84675749565923, -240.95890799347526, -129.60736066380215, 244.1017963518025, 2401.7765426255482, -0.0012353160968462526, -1200.889231261518, -2079.999379580687, -1200.8871219298928, 2080.0002098227765, 278.3639927215926, -19.442152546554286, -151.48315900531122, -250.58060785154075, -126.22329904196826, 244.78943118737322)
    
