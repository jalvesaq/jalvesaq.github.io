// --- 1. Tab Router Engine ---
document.querySelectorAll('.nav-btn').forEach(button => {
    button.addEventListener('click', function(e) {
        e.preventDefault();

        // Remove active states
        document.querySelectorAll('.nav-btn').forEach(btn => btn.classList.remove('active'));
        document.querySelectorAll('section').forEach(sec => sec.classList.remove('active'));

        // Apply active states to target
        this.classList.add('active');
        const targetId = this.getAttribute('data-target');
        document.getElementById(targetId).classList.add('active');

        // Push cleanly to browser URL state history
        history.pushState(null, null, `#${targetId}`);
    });
});

// Sync landing view state with incoming deep link address hashes
window.addEventListener('DOMContentLoaded', () => {
    const hash = window.location.hash.replace('#', '');
    if (hash) {
        const targetBtn = document.querySelector(`[data-target="${hash}"]`);
        if (targetBtn) targetBtn.click();
    }
});

// --- 2. String translation ---
let darkTxt = '🌙  Dark';
let lightTxt = '☀️ Light';
if (document.documentElement.lang === 'pt') {
    darkTxt = '🌙  Escuro';
    lightTxt = '☀️ Claro';
}

// --- 3. Dark/Light State Manager ---
const themeToggleBtn = document.getElementById('themeToggle');
const currentTheme = localStorage.getItem('theme');

// Apply stored configuration on reload
if (currentTheme === 'light') {
    document.documentElement.setAttribute('data-theme', 'light');
    themeToggleBtn.textContent = darkTxt;
} else {
    document.documentElement.setAttribute('data-theme', 'dark');
    themeToggleBtn.textContent = lightTxt;
}

// Click state observer loop
themeToggleBtn.addEventListener('click', () => {
    if (document.documentElement.getAttribute('data-theme') !== 'dark') {
        document.documentElement.setAttribute('data-theme', 'dark');
        themeToggleBtn.textContent = lightTxt;
        localStorage.setItem('theme', 'dark');
    } else {
        document.documentElement.setAttribute('data-theme', 'light');
        themeToggleBtn.textContent = darkTxt;
        localStorage.setItem('theme', 'light');
    }
});

async function onLoad() {
    const mi = document.getElementById('minfo');
    const m = atob("bWFpbHRvOg==");
    const a = atob("amFhQHVmYy5icgo=");
    const r = atob("cmIuY2Z1QGFhago=");
    mi.href = m + a;
    mi.textContent = r;
}

window.addEventListener('DOMContentLoaded', onLoad);
