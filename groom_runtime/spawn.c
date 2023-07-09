#include <groom/intrinsics.h>
#include <groom/spawn.h>

#define CORE_MAX 32

#define MIN(x, y) ((x) < (y) ? (x) : (y))

struct spawn_task_args {
    gpu_task_func_t func;
    void* arg;
    int offset;
    int full_warps;
    int rem_warps;
    int num_warps;
};

static void* g_spawn_args[CORE_MAX];

static void spawn_task_thread_main(void)
{
    int core_id = gpu_core_id();
    int wid = gpu_warp_id();
    int tid = gpu_thread_id();
    int NT = gpu_num_threads();
    struct spawn_task_args* spawn_args = g_spawn_args[core_id];
    int wK;
    int tK;
    int offset;

    wK = (spawn_args->full_warps * wid) + MIN(spawn_args->rem_warps, wid);
    tK = spawn_args->full_warps + (wid < spawn_args->rem_warps);
    offset = spawn_args->offset + (wK * NT) + (tid * tK);

    for (int task_id = offset, N = task_id + tK; task_id < N; task_id++)
        spawn_args->func(task_id, spawn_args->arg);

    gpu_barrier(0, spawn_args->num_warps);
}

static void spawn_task_wspawn_callback()
{
    int wid = gpu_warp_id();

    gpu_tmc(-1);

    spawn_task_thread_main();

    gpu_tmc(wid == 0);
}

static void spawn_task_rem(int tmask)
{
    int core_id = gpu_core_id();
    int tid = gpu_thread_id();
    struct spawn_task_args* spawn_args = g_spawn_args[core_id];

    gpu_tmc(tmask);

    spawn_args->func(spawn_args->offset + tid, spawn_args->arg);

    gpu_tmc(1);
}

void gpu_spawn_tasks(int num_tasks, gpu_task_func_t func, void* arg)
{
    struct spawn_task_args spawn_args;
    int core_id = gpu_core_id();
    int NC = gpu_num_cores();
    int NW = gpu_num_warps();
    int NT = gpu_num_threads();
    int WT = NW * NT;
    int nC;
    int nW;
    int tasks_per_core, real_tasks_per_core;
    int fW, rW;
    int rT;

    if (core_id >= CORE_MAX) return;

    nC = (num_tasks > WT) ? (num_tasks / WT) : 1;
    if (nC > NC) nC = NC;
    if (core_id >= nC) return;

    tasks_per_core = num_tasks / nC;
    real_tasks_per_core = tasks_per_core;
    if (core_id == nC - 1) {
        int rem = num_tasks - (nC * tasks_per_core);
        real_tasks_per_core += rem;
    }

    nW = real_tasks_per_core / NT;
    rT = real_tasks_per_core - (nW * NT);
    fW = (nW >= NW) ? (nW / NW) : 0;
    rW = (fW != 0) ? (nW - fW * NW) : 0;
    if (fW == 0) fW = 1;

    spawn_args.func = func;
    spawn_args.arg = arg;
    spawn_args.offset = core_id * tasks_per_core;
    spawn_args.full_warps = fW;
    spawn_args.rem_warps = rW;
    g_spawn_args[core_id] = &spawn_args;

    if (nW >= 1) {
        int nw = nW;
        if (nw > NW) nw = NW;

        spawn_args.num_warps = nw;
        gpu_wspawn(nw, spawn_task_wspawn_callback);
        spawn_task_wspawn_callback();
    }

    if (rT != 0) {
        int tmask = (1 << rT) - 1;

        spawn_args.offset = core_id * tasks_per_core + real_tasks_per_core - rT;
        spawn_task_rem(tmask);
    }
}
