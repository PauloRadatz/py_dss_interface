# -*- encoding: utf-8 -*-
"""
 Created by Ênio Viana at 15/05/2021
"""
from py_dss_interface.models.Example.ExampleBase import ExampleBase

dss = ExampleBase("13").dss

# Integer methods
print(45 * '=' + ' Integer Methods' + 45 * '=')
print(f'dss.parallel_num_cpus(): {dss.parallel_num_cpus()}')
print(f'dss.parallel_num_cores(): {dss.parallel_num_cores()}')
print(f'dss.parallel_read_active_actor(): {dss.parallel_read_active_actor()}')
print(f'dss.parallel_write_active_actor(): {dss.parallel_write_active_actor(1)}')
print(f'dss.parallel_read_actor_cpu(): {dss.parallel_read_actor_cpu()}')
print(f'dss.parallel_create_actor(): {dss.parallel_create_actor()}')
print(f'dss.parallel_read_actor_cpu(): {dss.parallel_read_actor_cpu()}')
print(f'dss.parallel_write_actor_cpu(): {dss.parallel_write_actor_cpu(1)}')
print(f'dss.parallel_num_actors(): {dss.parallel_num_actors()}')
print(f'dss.parallel_wait(): {dss.parallel_wait()}')
print(f'dss.parallel_read_active_parallel(): {dss.parallel_read_active_parallel()}')
print(f'dss.parallel_write_active_parallel(): {dss.parallel_write_active_parallel(1)}')
print(f'dss.parallel_read_active_parallel(): {dss.parallel_read_active_parallel()}')
print(f'dss.parallel_read_concatenate_reportsl(): {dss.parallel_read_concatenate_reportsl()}')
print(f'dss.parallel_write_concatenate_reportsl(): {dss.parallel_write_concatenate_reportsl(1)}')

# Variant methods
print(45 * '=' + ' Variant Methods' + 45 * '=')
print(f'dss.parallel_actor_progress(): {dss.parallel_actor_progress()}')
print(f'dss.parallel_actor_status(): {dss.parallel_actor_status()}')
