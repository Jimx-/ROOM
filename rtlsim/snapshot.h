#ifndef _RTLSIM_SNAPSHOT_H_
#define _RTLSIM_SNAPSHOT_H_

#include <cstdint>
#include <cstddef>
#include <deque>
#include <sys/types.h>

namespace room {

struct Subprocess {
    enum class ExitStatus : uint32_t {
        SUCCESS = 0,
        FAILURE = 1,
    };

    pid_t pid;
    int pfd[2];

    void wait();
    void send(ExitStatus status);
    ExitStatus receive();
};

class ForkSnapshot {
public:
    ForkSnapshot(uint64_t fork_interval, size_t max_children = 2);

    bool is_child() const { return is_child_; }
    bool should_stop() const { return should_stop_; }

    bool tick();

    void continue_from_child();
    void finalize();

private:
    uint64_t fork_interval_;
    size_t max_children_;
    uint64_t cycle_, last_fork_cycle_;
    bool is_child_;
    bool should_stop_;

    std::deque<Subprocess> children_;

    bool fork_child();
    void kill_first_child();
};

} // namespace room

#endif
