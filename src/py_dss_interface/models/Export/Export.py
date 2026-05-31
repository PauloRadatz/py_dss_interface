# -*- encoding: utf-8 -*-

from py_dss_interface.models.Export.ExportV import ExportV


class Export(ExportV):
    """Implements the OpenDSS Export interface.

    Returns the same data as the OpenDSS ``export`` commands, but in-memory
    (no CSV file is created). The raw CSV text is returned as-is; parsing is
    left to the consumer (e.g. py-dss-toolkit).
    """

    def __init__(self, obj_dss):
        super().__init__(obj_dss)

    @property
    def voltages_ln(self) -> str:
        """Raw CSV text equivalent to the ``export voltages`` command (bus/nodal
        line-to-neutral voltages), returned in-memory instead of being written to a file.

        Returns:
            str: CSV text with header ``Bus, BasekV, Node1, Magnitude1, Angle1,
            pu1, ...`` and one row per bus.
        """
        return ExportV._voltages_ln(self)

    @property
    def voltages_ll(self) -> str:
        """Raw CSV text for line-to-line bus voltages (nodes 12, 23, 31),
        returned in-memory instead of being written to a file.

        Returns:
            str: CSV text with header ``Bus, BasekV, Node1, Magnitude1, Angle1,
            pu1, ...`` where node labels are 12, 23, 31 for L-L pairs.
        """
        return ExportV._voltages_ll(self)

    @property
    def elem_voltages(self) -> str:
        """Raw CSV text for element conductor voltages (enhanced ``export elemvoltages``
        format with Node_i, V_i, Ang_i, Vpu_i per conductor), returned in-memory.

        Returns:
            str: CSV text with header ``Element, Nterminals, Nconductors,
            Node_1, V_1, Ang_1, Vpu_1, ...`` and one row per enabled element.
        """
        return ExportV._elem_voltages(self)
