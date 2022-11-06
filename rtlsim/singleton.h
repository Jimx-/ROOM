#ifndef _RTLSIM_SINGLETON_H_
#define _RTLSIM_SINGLETON_H_

namespace room {

template <typename T> class Singleton {
public:
    Singleton() { m_singleton = static_cast<T*>(this); }

    Singleton(const Singleton<T>&) = delete;
    Singleton<T>& operator=(const Singleton<T>&) = delete;

    ~Singleton() { m_singleton = nullptr; }

    static T& get_singleton() { return *m_singleton; }

    static T* get_singleton_ptr() { return m_singleton; }

protected:
    static T* m_singleton;
};

} // namespace room

#endif
