# -*- encoding: utf-8 -*-
"""
 Created by eniocc at 11/10/2020
"""
from typing import List

from py_dss_interface.models.Generators.GeneratorsF import GeneratorsF
from py_dss_interface.models.Generators.GeneratorsI import GeneratorsI
from py_dss_interface.models.Generators.GeneratorsS import GeneratorsS
from py_dss_interface.models.Generators.GeneratorsV import GeneratorsV


class Generators(GeneratorsI, GeneratorsF, GeneratorsS, GeneratorsV):
    """
    This interface implements the Generators (IGenerators) interface of OpenDSS by declaring 4 procedures for
    accessing the different properties included in this interface: GeneratorsI, GeneratorsF, GeneratorsS, GeneratorsV.
    """

    def __init__(self, dss_obj):
        super().__init__(dss_obj)

    @property
    def count(self) -> int:
        """Returns the number of generators Objects in Active Circuit."""
        return GeneratorsI._count(self)

    def first(self) -> int:
        """Sets first generator to be active. Returns 0 if None."""
        return GeneratorsI._first(self)

    @property
    def forced_on(self) -> int:
        """Returns 1 if the generator is forced ON regardless of other dispatch criteria; otherwise, returns 0.

        Allows to force ON regardless of other dispatch criteria. To force ON put 1 in the argument, otherwise put
                0. """
        return GeneratorsI._forced_on(self)

    @forced_on.setter
    def forced_on(self, value: int):
        GeneratorsI._forced_on_write(self, value)

    @property
    def idx(self) -> int:
        """Gets the active generator by Index into generators list. 1..Count.

        Sets the active generator (argument) by Index into generators list. 1..Count."""
        return GeneratorsI._idx(self)

    @idx.setter
    def idx(self, value: int):
        GeneratorsI._idx_write(self, value)

    @property
    def kv(self) -> float:
        """Gets the voltage base for the active generator, kV.

        Sets the voltage base for the active generator, kV."""
        return GeneratorsF._kv(self)

    @kv.setter
    def kv(self, value: float):
        GeneratorsF._kv_write(self, value)

    @property
    def kva(self) -> float:
        """Gets the KVA rating of the generator.

        Sets the KVA rating of the generator."""
        return GeneratorsF._kva_rated(self)

    @kva.setter
    def kva(self, value: float):
        GeneratorsF._kva_rated_write(self, value)

    @property
    def kvar(self) -> float:
        """Gets the kvar output for the active generator, kW is updated for current power factor.

        Sets the kvar output for the active generator, kW is updated for current power factor."""
        return GeneratorsF._kvar(self)

    @kvar.setter
    def kvar(self, value: float):
        GeneratorsF._kvar_write(self, value)

    @property
    def kw(self) -> float:
        """Gets the kW output for the active generator, kvar is updated for current power factor.

        Sets the kW output for the active generator, kvar is updated for current power factor."""
        return GeneratorsF._kw(self)

    @kw.setter
    def kw(self, value: float):
        GeneratorsF._kw_write(self, value)

    @property
    def model(self) -> int:
        """Gets the active generator Model (see Manual for details).
                1:Generator injects a constant kW at specified power factor.
                2:Generator is modeled as a constant admittance.
                3:Const kW, constant kV.  Somewhat like a conventional transmission power flow P-V generator.
                4:Const kW, Fixed Q (Q never varies)
                5:Const kW, Fixed Q(as a constant reactance)
                6:Compute load injection from User-written Model.(see usage of Xd, Xdp)
                7:Constant kW, kvar, but current-limited below Vminpu. Approximates a simple inverter. See also Balanced.

        Sets the active generator Model (see Manual for details).
                1:Generator injects a constant kW at specified power factor.
                2:Generator is modeled as a constant admittance.
                3:Const kW, constant kV.  Somewhat like a conventional transmission power flow P-V generator.
                4:Const kW, Fixed Q (Q never varies)
                5:Const kW, Fixed Q(as a constant reactance)
                6:Compute load injection from User-written Model.(see usage of Xd, Xdp)
                7:Constant kW, kvar, but current-limited below Vminpu. Approximates a simple inverter. See also Balanced.
                """
        return GeneratorsI._model(self)

    @model.setter
    def model(self, value: int):
        GeneratorsI._model_write(self, value)

    @property
    def name(self) -> str:
        """Gets the name of the active Generator.

        Sets the name of the active Generator."""
        return GeneratorsS._name(self)

    @name.setter
    def name(self, value: str):
        GeneratorsS._name_write(self, value)

    @property
    def names(self) -> List[str]:
        """Gets the array of names of all Generator objects."""
        return GeneratorsV._names(self)

    def next(self) -> int:
        """Sets next generator to be active. Returns 0 if None."""
        return GeneratorsI._next(self)

    @property
    def pf(self) -> float:
        """Gets the power factor (pos. = producing vars). Updates kvar based on present kW value.

        Sets the power factor (pos. = producing vars). Updates kvar based on present kW value."""
        return GeneratorsF._pf(self)

    @pf.setter
    def pf(self, value: float):
        GeneratorsF._pf_write(self, value)

    @property
    def phases(self) -> int:
        return GeneratorsI._phases(self)

    @phases.setter
    def phases(self, value: int):
        """Returns the number of phases of the active generator.

        Sets the number of phases (argument) of the active generator."""
        GeneratorsI._phases_write(self, value)

    @property
    def register_names(self) -> List[str]:
        """Gets the array of names of all generator Energy Meter registers."""
        return GeneratorsV._register_names(self)

    @property
    def register_values(self) -> List[float]:
        """Gets the array of values in generator Energy Meter registers."""
        return GeneratorsV._register_values(self)

    @property
    def vmax_pu(self) -> float:
        """Gets the Vmaxpu for Generator Model.

        Sets the Vmaxpu for Generator Model."""
        return GeneratorsF._vmax_pu(self)

    @vmax_pu.setter
    def vmax_pu(self, value: float):
        GeneratorsF._vmax_pu_write(self, value)

    @property
    def vmin_pu(self) -> float:
        """Gets the Vminpu for Generator Model.

        Sets the Vminpu for Generator Model."""
        return GeneratorsF._vmin_pu(self)

    @vmin_pu.setter
    def vmin_pu(self, value: float):
        GeneratorsF._vmin_pu_write(self, value)
