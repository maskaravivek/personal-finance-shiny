// www/descope-auth.js
Shiny.addCustomMessageHandler('initDescopeAuth', function (config) {
    const wcElement = document.createElement('descope-wc');
    wcElement.setAttribute('project-id', config.projectId);
    wcElement.setAttribute('flow-id', config.flowId);
    wcElement.setAttribute('theme', config.theme);

    wcElement.addEventListener('success', function (e) {
        const user = e.detail.user;
        console.log('User authenticated:', user);
        Shiny.setInputValue(config.successInput, user);
    });

    wcElement.addEventListener('error', function (err) {
        console.error('Authentication error:', err);
        Shiny.setInputValue(config.errorInput, err);
    });

    const container = document.getElementById(config.containerId);
    if (container) {
        container.innerHTML = ''; // Clear any previous instances
        container.appendChild(wcElement);
    }
});
