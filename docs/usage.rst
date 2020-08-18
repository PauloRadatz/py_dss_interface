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

    (57499.99984854059, 33197.64043666635, -9.850168857798125e-06, -66395.28082241151, -57499.99983869039, 33197.64039870235, 2521.7380661242582, -0.033332433156079276, -1245.8787300524257, -2157.924347023293, -1260.82801155609, 2183.904446042037, 2447.0051283472344, -21.90789306454855, -1219.3723934741943, -2138.9560914885365, -1206.3038367162885, 2085.3954297178566, 2429.501899704568, -31.198358333109713, -1203.2056471193102, -2152.310316449565, -1195.7215248567497, 2055.0852535673275, 2391.084320783626, -50.062131096842805, -1173.690262916864, -2184.4446438534164, -1188.2054759675134, 1996.7915167349508, 2400.2310288860003, -4.936891840982341, -1132.6674904482488, -2201.9831980231393, -1234.9597193512514, 1973.2329294055412, 2439.72672684693, -24.570582224732526, -1218.7845460017886, -2133.897053719238, -1202.9249506150766, 2080.0210888156566, -1205.0143876822408, 2080.9143155817396, -1215.2647596418246, -2115.8095371195886, -1204.2592626597598, 2075.7516204362637, -1216.061165717514, -2110.6466988639604, 2390.808195032073, -50.22915289202546, -1173.7854073662652, -2184.432600932979, -1187.9686135815562, 1996.6985720515652, 2375.2196161407214, -60.36283167374468, -1182.8077277041098, -2185.8222050310314, -1186.0448159963867, 1992.4002359846927, 2386.353397345648, -50.914573469570655, -1182.200990121016, 1994.7177220557478, -1174.6338021812282, 1993.5752272005625, 2373.0662121128853, -47.62346752050522, 274.7965425038391, -6.024932145551775, -140.047848266581, -240.58126637198527, -134.11350748578673, 236.5815972605969, 2401.7759437795626, -0.0004178201077101414, -1200.8882119836212, -2079.9992432513623, -1200.8874940740557, 2079.9991885692953, 267.7345981151209, 59.996080180480014, -74.74933913449347, -273.6209533797813, -189.79998261133724, 185.5877875805238)
    
