# -*- coding: utf-8 -*-
# @Time    : 3/10/2023 10:59 AM
# @Author  : Paulo Radatz
# @Email   : paulo.radatz@gmail.com
# @File    : Error.py
# @Software: PyCharm


class Error:

    # ANSI escape codes for text styling and colors
    RED = "\033[91m"
    GREEN = "\033[92m"
    YELLOW = "\033[93m"
    BRIGHT = "\033[1m"
    RESET = "\033[0m"

    @staticmethod
    def use_package_v1():
        message_1 = f"\n{Error.RED}{Error.BRIGHT}py-dss-interface version issue{Error.RESET}"

        message_2 = f"{Error.GREEN}{Error.BRIGHT}There are two solutions:{Error.RESET}"

        message_3 = """
            1 - Go to terminal and type: pip install py-dss-interface==1.0.2
            2 - Change py-dss-interface.DSSDLL to py-dss-interface.DSS and update your entire code according to the new syntax.
        """

        message_4 = f"{Error.YELLOW}{Error.BRIGHT}Why do I have this problem?{Error.RESET}"
        message_5 = """
            py-dss-interface version 2 or above does not have backward compatibility with version 1.
            Your code uses py-dss-interface 1.0.2, but you have version 2 or above."""

        print(message_1)
        print(message_2)
        print(message_3)
        print(message_4)
        print(message_5)

    @staticmethod
    def method_not_working(method):
        message_1 = f"\n{Error.RED}{Error.BRIGHT}Method issue{Error.RESET}"
        message_2 = f"""
            The {method} is not currently working.
            You can try to use a workaround, but if it is impossible, please get in touch with Paulo Radatz."""
        print(message_1)
        print(message_2)

    @staticmethod
    def linux_version():
        message_1 = f"\n{Error.RED}{Error.BRIGHT}Operation System Error{Error.RESET}"
        message_2 = f"""
                    py-dss-interface uses only the official version of OpenDSS.
                    EPRI provides OpenDSS only for Windows machines. """

        print(message_1)
        print(message_2)
