#ifndef _GROOM_SPAWN_H_
#define _GROOM_SPAWN_H_

typedef void (*gpu_task_func_t)(int task_id, void* arg);

void gpu_spawn_tasks(int num_tasks, gpu_task_func_t func, void* arg);

#endif
