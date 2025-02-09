#include "snapshot.h"

#include <sys/wait.h>

#include "spdlog/spdlog.h"

namespace room {

void Subprocess::wait()
{
    int status;
    ::waitpid(pid, &status, 0);
}

void Subprocess::send(ExitStatus status)
{
    uint32_t val = static_cast<uint32_t>(status);
    ::write(pfd[1], &val, sizeof(val));
}

Subprocess::ExitStatus Subprocess::receive()
{
    uint32_t val;

    for (;;) {
        ssize_t ret = ::read(pfd[0], &val, sizeof(val));
        if (ret > 0) break;

        if (errno == EINTR || errno == EAGAIN) continue;

        spdlog::error("Failed to read status from parent: {}", strerror(errno));
        return ExitStatus::SUCCESS;
    }

    return static_cast<ExitStatus>(val);
}

ForkSnapshot::ForkSnapshot(uint64_t fork_interval, size_t max_children)
    : fork_interval_(fork_interval), max_children_(max_children), cycle_(0),
      last_fork_cycle_(0), is_child_(false), should_stop_(false)
{}

bool ForkSnapshot::tick()
{
    cycle_++;

    if (is_child_) return false;

    if (cycle_ - last_fork_cycle_ < fork_interval_) {
        return false;
    }

    bool forked = fork_child();
    if (forked) {
        last_fork_cycle_ = cycle_;
    }

    return forked;
}

bool ForkSnapshot::fork_child()
{
    if (children_.size() >= max_children_) {
        kill_first_child();
    }

    Subprocess child;
    ::pipe(child.pfd);
    int pid = fork();

    if (pid < 0) {
        spdlog::error("Failed to create child process");
        return false;
    }

    if (pid > 0) {
        // Parent
        child.pid = pid;
        ::close(child.pfd[0]);
        child.pfd[0] = -1;

        children_.emplace_back(std::move(child));

        spdlog::debug("Created child process {}", pid);
    } else {
        // Child
        is_child_ = true;
        children_.clear();
        ::close(child.pfd[1]);

        auto status = child.receive();
        if (status == Subprocess::ExitStatus::SUCCESS) {
            should_stop_ = true;
        } else {
            spdlog::info("Continue in child process from cycle {}", cycle_);
        }
    }

    return true;
}

void ForkSnapshot::kill_first_child()
{
    if (children_.empty()) return;

    Subprocess& child = children_.front();

    child.send(Subprocess::ExitStatus::SUCCESS);
    child.wait();
    ::close(child.pfd[1]);

    spdlog::debug("Killed child process {}", child.pid);

    children_.pop_front();
}

void ForkSnapshot::continue_from_child()
{
    if (children_.empty()) return;

    Subprocess& child = children_.front();

    child.send(Subprocess::ExitStatus::FAILURE);
    child.wait();
    ::close(child.pfd[1]);

    children_.pop_front();
}

void ForkSnapshot::finalize()
{
    while (!children_.empty()) {
        kill_first_child();
    }
}

} // namespace room
