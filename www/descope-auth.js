// www/descope-auth.js

// 1) Initialize the Descope authentication flow
Shiny.addCustomMessageHandler('initDescopeAuth', function (config) {
    const wcElement = document.createElement('descope-wc');
    wcElement.setAttribute('project-id', config.projectId);
    wcElement.setAttribute('flow-id', config.flowId);
    wcElement.setAttribute('theme', config.theme);

    wcElement.addEventListener('success', function (e) {
        const user = e.detail.user;
        console.log('User authenticated:', user);
        // Pass user object to Shiny
        Shiny.setInputValue(config.successInput, user);
    });

    wcElement.addEventListener('error', function (err) {
        console.error('Authentication error:', err);
        Shiny.setInputValue(config.errorInput, err);
    });

    const container = document.getElementById(config.containerId);
    if (container) {
        container.innerHTML = ''; // Clear previous instance
        container.appendChild(wcElement);
    }
});

Shiny.addCustomMessageHandler('descopeLogout', function () {
    const wcElement = document.querySelector('descope-wc');
    if (wcElement) {
        wcElement.logout();
    }
});
